defmodule Pigeon.APNS.Worker do
  @moduledoc """
  Handles all APNS request and response parsing over an HTTP2 connection.
  """
  use GenServer
  require Logger

  alias Pigeon.APNS

  defp apns_production_api_uri, do: "api.push.apple.com"
  defp apns_development_api_uri, do: "api.development.push.apple.com"

  def push_uri(mode) do
    case mode do
      :dev -> apns_development_api_uri()
      :prod -> apns_production_api_uri()
    end
  end

  def start_link(config) do
    case config[:name] do
      nil -> GenServer.start_link(__MODULE__, {:ok, config})
      name -> GenServer.start_link(__MODULE__, {:ok, config}, name: name)
    end
  end

  def stop, do: :gen_server.cast(self(), :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    mode = config[:mode]
    case connect_socket(config, 0) do
      {:ok, socket} ->
        Process.send_after(self(), :ping, config[:ping_period])
        {:ok, %{
          apns_socket: socket,
          mode: mode,
          reconnect: Map.get(config, :reconnect, true),
          config: config,
          stream_id: 1,
          queue: %{}
        }}
      {:closed, _socket} ->
        Logger.error """
          Socket closed unexpectedly.
          """
        {:stop, {:error, :bad_connection}}
      {:error, :timeout} ->
        Logger.error """
          Failed to establish SSL connection. Is the certificate signed for :#{mode} mode?
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
    uri = config[:mode] |> push_uri |> to_char_list
    case connect_socket_options(config) do
      {:ok, options} -> do_connect_socket(config, uri, options, tries)
      error -> error
    end
  end

  def connect_socket_options(config) do
    cert = get_opt(config, :cert, :certfile)
    key = get_opt(config, :key, :keyfile)
    cond do
      cert && key ->
        options =
          [cert,
          key,
          {:password, ''},
          {:packet, 0},
          {:reuseaddr, true},
          {:active, true},
          :binary]
          |> optional_add_2197(config)
        {:ok, options}
      true ->
        {:error, :invalid_config}
    end
  end

  defp optional_add_2197(options, config) do
    case config[:use_2197] do
      true -> options ++ [{:port, 2197}]
      _ -> options
    end
  end

  defp get_opt(config, key_1, key_2) do
    cond do
      config[key_1] -> {key_1, config[key_1]}
      config[key_2] -> {key_2, config[key_2]}
      true -> nil
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

  def handle_cast({:push, :apns, notification}, state) do
    send_push(state, notification, nil)
  end

  def handle_cast({:push, :apns, notification, on_response}, state) do
    send_push(state, notification, on_response)
  end

  def handle_cast(msg, state) do
    Logger.debug "Recv: #{inspect(msg)}"
    {:noreply, state}
  end

  def send_push(state, notification, on_response) do
    %{apns_socket: socket, stream_id: stream_id, queue: queue} = state
    json = Pigeon.Notification.json_payload(notification.payload)
    req_headers = [
      {":method", "POST"},
      {":path", "/3/device/#{notification.device_token}"},
      {"content-length", "#{byte_size(json)}"}]
      |> put_apns_id(notification)
      |> put_apns_topic(notification)

    Pigeon.Http2.Client.default().send_request(socket, req_headers, json)
    new_q = Map.put(queue, "#{stream_id}", {notification, on_response})
    new_stream_id = stream_id + 2
    {:noreply, %{state | stream_id: new_stream_id, queue: new_q}}
  end

  defp put_apns_id(headers, notification) do
    case notification.id do
      nil -> headers
      id -> headers ++ [{"apns-id", id}]
    end
  end

  defp put_apns_topic(headers, notification) do
    case notification.topic do
      nil   -> headers
      topic -> headers ++ [{"apns-topic", topic}]
    end
  end

  def handle_info(:ping, state) do
    Pigeon.Http2.Client.default().send_ping(state.apns_socket)
    Process.send_after(self(), :ping, state.config.ping_period)

    {:noreply, state}
  end

  def handle_info({:closed, _}, state) do
    case state[:reconnect] do
      false ->
        Process.exit(state.apns_socket, :kill)
        {:stop, :normal, state}
      _     -> {:noreply, state}
    end
  end

  def handle_info(msg, state) do
    case Pigeon.Http2.Client.default().handle_end_stream(msg, state) do
      {:ok, %Pigeon.Http2.Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  def process_end_stream(%Pigeon.Http2.Stream{id: stream_id, headers: headers, body: body},
                                            %{apns_socket: _socket, queue: queue} = state) do

    {notification, on_response} = queue["#{stream_id}"]

    case get_status(headers) do
      "200" ->
        notification = %{notification | id: get_apns_id(headers)}
        unless on_response == nil do on_response.({:ok, notification}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      nil ->
        {:noreply, state}
      _error ->
        reason = APNS.Error.parse(body)
        APNS.Error.log(reason, notification)
        unless on_response == nil do on_response.({:error, reason, notification}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
    end
  end

  defp get_status(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == ":status" end) do
      {":status", status} -> status
      nil -> nil
    end
  end

  defp get_apns_id(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == "apns-id" end) do
      {"apns-id", id} -> id
      nil -> nil
    end
  end
end
