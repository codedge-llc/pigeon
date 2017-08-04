defmodule Pigeon.FCM.Worker do
  @moduledoc """
  Handles all FCM request and response parsing over an HTTP2 connection.
  """
  use GenServer
  require Logger

  alias Pigeon.FCM.{NotificationResponse, ResultParser}

  @default_ping_period 600_000 # 10 minutes

  defp fcm_uri(config), do: config[:endpoint] || 'fcm.googleapis.com'

  def start_link(config) do
    case config[:name] do
      nil -> GenServer.start_link(__MODULE__, {:ok, config})
      name -> GenServer.start_link(__MODULE__, {:ok, config}, name: name)
    end
  end

  def stop, do: :gen_server.cast(self(), :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    case connect_socket(config, 0) do
      {:ok, socket} ->
        ping = config[:ping_period] || @default_ping_period
        config = Map.put(config, :ping_period, ping)
        Process.send_after(self(), :ping, ping)

        {:ok, %{
          socket: socket,
          key: config[:key],
          stream_id: 1,
          queue: %{},
          config: config
        }}
      {:closed, _socket} ->
        Logger.error """
          Socket closed unexpectedly.
          """
        {:stop, {:error, :bad_connection}}
      {:error, :timeout} ->
        Logger.error """
          Failed to establish SSL connection.
          """
        {:stop, {:error, :timeout}}
      {:error, :invalid_config} ->
        Logger.error """
          Invalid configuration.
          """
        {:stop, {:error, :invalid_config}}
    end
  end

  def connect_socket(_config, 3), do: {:error, :timeout}
  def connect_socket(config, tries) do
    uri = config |> fcm_uri() |> to_char_list
    case connect_socket_options(config) do
      {:ok, options} -> do_connect_socket(config, uri, options, tries)
    end
  end

  def connect_socket_options(config) do
    opts = [
      {:active, :once},
      {:packet, :raw},
      {:reuseaddr, true},
      {:alpn_advertised_protocols, [<<"h2">>]},
      {:reconnect, false},
      :binary
    ]
    |> optional_add_port(config)

    {:ok, opts}
  end

  def optional_add_port(opts, config) do
    case config[:port] do
      nil -> opts
      port -> opts ++ [{:port, port}]
    end
  end

  defp do_connect_socket(config, uri, options, tries) do
    case Pigeon.Http2.Client.default().connect(uri, :https, options) do
      {:ok, socket} -> {:ok, socket}
      {:error, reason} ->
        Logger.error(inspect(reason))
        connect_socket(config, tries + 1)
    end
  end

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, :fcm, notification, on_response, %{key: key}}, state) do
    send_push(state, notification, on_response, key)
  end
  def handle_cast({:push, :fcm, notification, on_response, _opts}, state) do
    send_push(state, notification, on_response)
  end

  def handle_cast(msg, state) do
    Logger.debug "Recv: #{inspect(msg)}"
    {:noreply, state}
  end

  def send_push(%{key: key} = state, payload, on_response) do
    send_push(state, payload, on_response, key)
  end

  def send_push(%{socket: socket, queue: queue} = state,
                {registration_ids, payload},
                on_response,
                key) do

    state =
      case socket do
        nil ->
          {_, state} = reconnect(state)
          state
        _socket -> state
      end

    req_headers = [
      {":method", "POST"},
      {":path", "/fcm/send"},
      {"authorization", "key=#{key}"},
      {"content-type", "application/json"},
      {"accept", "application/json"}
    ]

    Pigeon.Http2.Client.default().send_request(state.socket, req_headers, payload)

    new_q = Map.put(queue, "#{state.stream_id}", {registration_ids, on_response})
    new_stream_id = state.stream_id + 2
    {:noreply, %{state | stream_id: new_stream_id, queue: new_q}}
  end

  def reconnect(%{config: config} = state) do
    case connect_socket(config, 0) do
      {:ok, new_socket} ->
        Process.send_after(self(), :ping, config.ping_period)
        {:ok, %{state | socket: new_socket, queue: %{}, stream_id: 1}}
      error ->
        error |> inspect() |> Logger.error
        {:error, state}
    end
  end

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  defp log_error(code, reason) do
    Logger.error("#{reason}: #{code}")
  end

  def handle_info(:ping, state) do
    if state.socket != nil do
      Pigeon.Http2.Client.default().send_ping(state.socket)
      Process.send_after(self(), :ping, state.config.ping_period)
    end

    {:noreply, state}
  end

  def handle_info({:ping, _from}, state), do: {:noreply, state}

  def handle_info({:closed, _from}, state) do
    {:noreply, %{state | socket: nil}}
  end

  def handle_info(msg, state) do
    case Pigeon.Http2.Client.default().handle_end_stream(msg, state) do
      {:ok, %Pigeon.Http2.Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  def process_end_stream(%Pigeon.Http2.Stream{id: stream_id, headers: headers, body: body, error: error} = stream,
                                            %{socket: _socket, queue: queue} = state) do

    cond do
      queue["#{stream_id}"] == nil ->
        Logger.error("Unknown stream_id: #{stream_id}, Error: #{inspect(error)}")
        {:noreply, state}
      error == nil ->
        {registration_ids, on_response} = queue["#{stream_id}"]
        case get_status(headers) do
          nil ->
            stream |> inspect() |> Logger.error
            new_queue = Map.delete(queue, "#{stream_id}")
            {:noreply, %{state | queue: new_queue}}
          "200" ->
            result = Poison.decode!(body)
            parse_result(registration_ids, result, on_response)
            new_queue = Map.delete(queue, "#{stream_id}")
            {:noreply, %{state | queue: new_queue}}
          "401" ->
            log_error("401", "Unauthorized")
            unless on_response == nil do on_response.({:error, :unauthorized}) end
            new_queue = Map.delete(queue, "#{stream_id}")
            {:noreply, %{state | queue: new_queue}}
          "400" ->
            log_error("400", "Malformed JSON")
            unless on_response == nil do on_response.({:error, :malformed_json}) end
            new_queue = Map.delete(queue, "#{stream_id}")
            {:noreply, %{state | queue: new_queue}}
          code ->
            reason = parse_error(body)
            log_error(code, reason)
            unless on_response == nil do on_response.({:error, reason}) end
            new_queue = Map.delete(queue, "#{stream_id}")
            {:noreply, %{state | queue: new_queue}}
        end
      true ->
        {_registration_ids, on_response} = queue["#{stream_id}"]
        error |> inspect() |> Logger.error
        unless on_response == nil do on_response.({:error, :unavailable}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
    end
  end

  # no on_response callback, ignore
  def parse_result(_, _, nil), do: :ok

  def parse_result(ids, %{"results" => results}, on_response) do
    ResultParser.parse(ids, results, on_response, %NotificationResponse{})
  end

  defp get_status(nil) do
    nil
  end
  defp get_status(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == ":status" end) do
      {":status", status} -> status
      nil -> nil
    end
  end
end
