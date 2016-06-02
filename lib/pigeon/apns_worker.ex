defmodule Pigeon.APNSWorker do
  @moduledoc """
    Handles all APNS request and response parsing over an HTTP2 connection.
  """
  alias Pigeon.{HTTP2}
  use GenServer
  require Logger

  def start_link(name, config) do
    GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  end

  def stop, do: :gen_server.cast(self, :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    mode = config[:mode]
    case connect_socket(mode, config) do
      {:ok, socket} ->
        establish_connection(mode, config, socket)
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

  def connect_socket(mode, %{cert: cert, certfile: nil, key: key, keyfile: nil}),
    do: connect_socket(mode, {:cert, cert}, {:key, key}, 0)
  def connect_socket(mode, %{cert: nil, certfile: certfile, key: key, keyfile: nil}),
    do: connect_socket(mode, {:certfile, certfile}, {:key, key}, 0)
  def connect_socket(mode, %{cert: nil, certfile: certfile, key: nil, keyfile: keyfile}),
    do: connect_socket(mode, {:certfile, certfile}, {:keyfile, keyfile}, 0)
  def connect_socket(mode, %{cert: cert, certfile: nil, key: nil, keyfile: keyfile}),
    do: connect_socket(mode, {:cert, cert}, {:keyfile, keyfile}, 0)
  def connect_socket(_mode, _), do: {:error, :invalid_config}

  def connect_socket(_mode, _config, 3), do: {:error, :timeout}
  def connect_socket(mode, cert, key, tries) do
    case HTTP2.connect(mode, cert, key) do
      {:ok, socket} -> {:ok, socket}
      {:error, _} -> connect_socket(mode, cert, key, tries + 1)
    end
  end

  def establish_connection(mode, config, socket) do
    state = %{
      apns_socket: socket,
      mode: mode,
      config: config,
      stream_id: 1
    }
    HTTP2.send_connection_preface(socket)
    HTTP2.establish_connection(socket)
    {:ok, state}
  end

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, :apns, notification}, state) do
    %{stream_id: stream_id} = state
    data = package_push(state, notification)
    send_push(data, state, notification, nil)
    { :noreply, %{state | stream_id: stream_id + 2 } }
  end

  def handle_cast({:push, :apns, notification, on_response}, state) do
    %{stream_id: stream_id} = state
    data = package_push(state, notification)
    send_push(data, state, notification, on_response)
    { :noreply, %{state | stream_id: stream_id + 2 } }
  end

  def package_push(state, notification) do
    %{apns_socket: socket, mode: mode, stream_id: stream_id} = state

    json = Pigeon.Notification.json_payload(notification.payload)
    push_header = HTTP2.push_header_frame(stream_id, mode, notification)
    push_data = HTTP2.push_data_frame(stream_id, json)
    {push_header, push_data}
  end

  def send_push({push_header, push_data}, state, notification, on_response) do
    %{apns_socket: socket, mode: mode, stream_id: stream_id} = state
    :ssl.send(socket, push_header)
    :ssl.send(socket, push_data)

    case HTTP2.wait_response socket do
      {:ok, headers, payload} ->
        process_response(payload, socket, notification, on_response)
      error -> error
    end
  end

  defp process_response(payload, socket, notification, on_response) do
    case HTTP2.status_code(payload) do
      200 ->
        unless on_response == nil do on_response.({:ok, notification}) end
      _error ->
        case HTTP2.wait_response socket do
        {:ok, _data_headers, data_payload} ->
          reason = parse_error(data_payload)
          log_error(reason, notification)
          unless on_response == nil do on_response.({:error, reason, notification}) end
        _ ->
          {:error, :timeout}
        end
    end
  end

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Mix.Utils.underscore |> String.to_atom
  end

  defp log_error(reason, notification) do
    Logger.error("#{reason}: #{error_msg(reason)}\n#{inspect(notification)}")
  end

  def error_msg(error) do
    case error do
      :payload_empty ->
        "The message payload was empty."
      :payload_too_large ->
        "The message payload was too large. The maximum payload size is 4096 bytes."
      :bad_topic ->
        "The apns-topic was invalid."
      :topic_disallowed ->
        "Pushing to this topic is not allowed."
      :bad_message_id ->
        "The apns-id value is bad."
      :bad_expiration_date ->
        "The apns-expiration value is bad."
      :bad_priority ->
        "The apns-priority value is bad."
      :missing_device_token ->
        """
        The device token is not specified in the request :path. Verify that the :path header
        contains the device token.
        """
      :bad_device_token ->
        """
        The specified device token was bad. Verify that the request contains a valid token and
        that the token matches the environment.
        """
      :device_token_not_for_topic ->
        "The device token does not match the specified topic."
      :unregistered ->
        "The device token is inactive for the specified topic."
      :duplicate_headers ->
        "One or more headers were repeated."
      :bad_certificate_environment ->
        "The client certificate was for the wrong environment."
      :bad_certificate ->
        "The certificate was bad."
      :forbidden ->
        "The specified action is not allowed."
      :bad_path ->
        "The request contained a bad :path value."
      :method_not_allowed ->
        "The specified :method was not POST."
      :too_many_requests ->
        "Too many requests were made consecutively to the same device token."
      :idle_timeout ->
        "Idle time out."
      :shutdown ->
        "The server is shutting down."
      :internal_server_error ->
        "An internal server error occurred."
      :service_unavailable ->
        "The service is unavailable."
      :missing_topic ->
          """
          The apns-topic header of the request was not specified and was required. The apns-topic
          header is mandatory when the client is connected using a certificate that supports
          multiple topics.
          """
      :timeout ->
        "The SSL connection timed out."
    end
  end

  def handle_info({:ssl, socket, bin}, state) do
    Logger.debug("Recv SSL data: #{inspect(bin)}")
    {:noreply, state}
  end

  def handle_info({:ssl_closed, _socket}, state) do
    %{config: config} = state
    Logger.debug("Got connection close...")
    initialize_worker(config)
  end
end
