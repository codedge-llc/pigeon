defmodule Pigeon.FCM.Worker do
  @moduledoc """
    Handles all APNS request and response parsing over an HTTP2 connection.
  """
  use GenServer
  require Logger

  alias Pigeon.FCM.NotificationResponse

  @ping_period 600_000 # 10 minutes

  defp fcm_uri(config), do: config[:endpoint] || 'fcm.googleapis.com'

  # def start_link(name, config) do
  #   GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  # end
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
        Process.send_after(self(), :ping, @ping_period)
        {:ok, %{
          fcm_socket: socket,
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
      error -> error
    end
  end

  def connect_socket_options(config) do
    opts = [
      {:active, :once},
      {:packet, :raw},
      {:reuseaddr, true},
      {:alpn_advertised_protocols, [<<"h2">>]},
      :binary
    ]
    # opts = [
    #   {:packet, 0},
    #   {:reuseaddr, true},
    #   {:active, true},
    #   :binary
    # ]
    |> optional_add_port(config)

    {:ok, []}
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

  def send_push(%{fcm_socket: socket, stream_id: stream_id, queue: queue} = state,
                {registration_ids, payload},
                on_response,
                key) do

    req_headers = [
      {":method", "POST"},
      {":path", "/fcm/send"},
      {"authorization", "key=#{key}"},
      {"content-type", "application/json"},
      {"accept", "application/json"}
    ]

    Pigeon.Http2.Client.default().send_request(socket, req_headers, payload)

    new_q = Map.put(queue, "#{stream_id}", {registration_ids, on_response})
    new_stream_id = stream_id + 2
    {:noreply, %{state | stream_id: new_stream_id, queue: new_q}}
  end

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  defp log_error(code, reason) do
    Logger.error("#{reason}: #{code}")
  end

  def handle_info(:ping, state) do
    Pigeon.Http2.Client.default().send_ping(state.fcm_socket)
    Process.send_after(self(), :ping, @ping_period)

    {:noreply, state}
  end

  def process_end_stream(%Pigeon.Http2.Stream{id: stream_id, headers: headers, body: body},
                                            %{fcm_socket: _socket, queue: queue} = state) do

    {registration_ids, on_response} = queue["#{stream_id}"]
    Logger.debug("#{inspect body} and #{inspect headers}")
    case get_status(headers) do
      "200" ->
        result =  Poison.decode! body
        parse_result(registration_ids, result, on_response)
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      nil ->
        {:noreply, state}
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
  end

  def handle_info({:ping, _from}, state), do: {:noreply, state}

  def handle_info({:closed, _from}, %{config: config} = state) do
    Logger.info "Reconnecting FCM client (Closed due to probable session_timed_out GOAWAY error)"
    case initialize_worker(config) do
      {:ok, newstate} -> {:noreply, newstate}
      error -> error
    end
  end

  def handle_info(msg, state) do
    case Pigeon.Http2.Client.default().handle_end_stream(msg, state) do
      {:ok, %Pigeon.Http2.Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  #def handle_info({:ok, _from}, state), do: {:noreply, state}

  # no on_response callback, ignore
  def parse_result(_, _, nil), do: :ok

  def parse_result(ids, %{"results" => results}, on_response) do
    parse_result1(ids, results, on_response, %NotificationResponse{})
  end

  def parse_result1([], [], on_response, result) do
    on_response.({:ok, result})
  end

  def parse_result1(regid, results, on_response, result) when is_binary(regid) do
    parse_result1([regid], results, on_response, result)
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id, "registration_id" => new_regid} | rest_results],
                    on_response,
                    %NotificationResponse{ update: update} =  resp) do

    new_updates = [{regid, new_regid} | update]
    parse_result1(reg_res, rest_results, on_response, %{resp | message_id: id, update: new_updates})
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id} | rest_results],
                    on_response,
                    %NotificationResponse{ok: ok} = resp) do

    parse_result1(reg_res, rest_results, on_response, %{resp | message_id: id, ok: [regid | ok]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => "Unavailable"} | rest_results],
                    on_response,
                    %NotificationResponse{retry: retry} = resp) do

    parse_result1(reg_res, rest_results, on_response, %{resp | retry: [regid | retry]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => invalid } | rest_results],
                    on_response,
                    %NotificationResponse{remove: remove} = resp) when invalid == "NotRegistered"
                                                                    or invalid == "InvalidRegistration" do

    parse_result1(reg_res, rest_results, on_response, %{resp | remove: [regid | remove]})
  end

  def parse_result1([regid | reg_res] = regs,
                    [%{"error" => error} | rest_results] = results,
                    on_response,
                    %NotificationResponse{error: regs_in_error} = resp) do

    case Map.has_key?(regs_in_error, error) do
      true ->
        parse_result1(reg_res, rest_results, on_response,
          %{resp | error: %{regs_in_error | error => regid}})
      false -> # create map key if required.
         parse_result1(regs, results, on_response,
          %{resp | error: Map.merge(%{error => []}, regs_in_error)})
    end
  end

  def parse_result1(regs, [%{"error" => error} | _r] = results, on_response,
      %NotificationResponse{error: errors} = resp) do

  end

  defp get_status(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == ":status" end) do
      {":status", status} -> status
      nil -> nil
    end
  end
end
