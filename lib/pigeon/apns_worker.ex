defmodule Pigeon.APNSWorker do  
  alias Pigeon.{HTTP2}
  use GenServer
  require Logger

  def start_link(name, mode, cert, key) do
    Logger.debug("Starting #{name}\n\t mode: #{mode}, cert: #{cert}, key: #{key}")
    GenServer.start_link(__MODULE__, {:ok, mode, cert, key}, name: name)
  end

  def stop() do
    :gen_server.cast(self, :stop)
  end

  def init({:ok, mode, cert, key} = args) do
    {:ok, socket} = Pigeon.HTTP2.connect(mode, cert, key)
    state = %{
      apns_socket: socket,
      mode: mode,
      cert: cert,
      key: key,
      stream_id: 1
    }
    {:ok, data} = Pigeon.HTTP2.send_connection_preface(socket)
    response = Pigeon.HTTP2.establish_connection(socket)
    {:ok, state}
  end

  def handle_call({:start_connection, mode, cert, key}, from, state) do
    c = Pigeon.APNS.Connection.new(mode, cert, key)
    state = %{
      apns_socket: c,
      mode: mode,
      cert: cert,
      key: key
    }
    {:noreply, state}
  end

  def handle_cast(:stop, state) do
    { :noreply, state }
  end

  def handle_cast({:push, :apns, notification}, state) do 
    %{apns_socket: socket, mode: mode, cert: cert, key: key, stream_id: stream_id} = state
    send_push(state, notification, nil)
    { :noreply, %{state | stream_id: stream_id + 2 } }
  end

  def handle_cast({:push, :apns, notification, on_response}, state) do 
    %{apns_socket: socket, mode: mode, cert: cert, key: key, stream_id: stream_id} = state
    send_push(state, notification, on_response)
    { :noreply, %{state | stream_id: stream_id + 2 } }
  end

  def send_push(state, notification, on_response) do
    %{apns_socket: socket, mode: mode, cert: cert, key: key, stream_id: stream_id} = state
    %{device_token: device_token, topic: topic, payload: payload} = notification

    push_header = Pigeon.HTTP2.push_header_frame(stream_id, mode, device_token, topic, payload)
    push_data = Pigeon.HTTP2.push_data_frame(stream_id, payload)

    :ssl.send(socket, push_header)
    :ssl.send(socket, push_data)

    {:ok, headers, payload} = HTTP2.wait_response socket
    headers |> HTTP2.parse_frame |> IO.inspect

    case HTTP2.status_code(payload) do
      200 ->
        unless on_response == nil do on_response.({:ok, notification}) end
      error ->
        {:ok, data_headers, data_payload} = HTTP2.wait_response socket
        reason = parse_error(data_payload)
        log_error(reason, notification)
        unless on_response == nil do on_response.({:error, reason, notification}) end
    end
  end

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> String.to_atom
  end

  defp log_error(reason, notification) do
    Logger.error("#{reason}: #{error_msg(reason)}\n\n#{inspect(notification)}")
  end

  def error_msg(error) do
		case error do
			:PayloadEmpty ->
				"The message payload was empty."
			:PayloadTooLarge ->
			 "The message payload was too large. The maximum payload size is 4096 bytes."
			:BadTopic ->
			 "The apns-topic was invalid."
			:TopicDisallowed ->
			 "Pushing to this topic is not allowed."
			:BadMessageId ->
			 "The apns-id value is bad."
			:BadExpirationDate ->
			 "The apns-expiration value is bad."
			:BadPriority ->
			 "The apns-priority value is bad."
			:MissingDeviceToken ->
			 "The device token is not specified in the request :path. Verify that the :path header contains the device token."
			:BadDeviceToken ->
			 "The specified device token was bad. Verify that the request contains a valid token and that the token matches the environment."
			:DeviceTokenNotForTopic ->
			 "The device token does not match the specified topic."
			:Unregistered ->
			 "The device token is inactive for the specified topic."
			:DuplicateHeaders ->
			 "One or more headers were repeated."
			:BadCertificateEnvironment ->
			 "The client certificate was for the wrong environment."
			:BadCertificate ->
			 "The certificate was bad."
			:Forbidden ->
			 "The specified action is not allowed."
			:BadPath ->
			 "The request contained a bad :path value."
			:MethodNotAllowed ->
			 "The specified :method was not POST."
			:TooManyRequests ->
			 "Too many requests were made consecutively to the same device token."
			:IdleTimeout ->
			 "Idle time out."
			:Shutdown ->
			 "The server is shutting down."
			:InternalServerError ->
			 "An internal server error occurred."
			:ServiceUnavailable ->
			 "The service is unavailable."
			:MissingTopic ->
				"The apns-topic header of the request was not specified and was required. The apns-topic header is mandatory when the client is connected using a certificate that supports multiple topics."
    end
  end

  def handle_info({:ssl_closed, socket}, %{apns_socket: socket, mode: mode, cert: cert, key: key} = state) do
    Logger.debug("Got connection close...")

    {:ok, sock} = Pigeon.HTTP2.connect(mode, cert, key)
    {:ok, data} = Pigeon.HTTP2.send_connection_preface(sock)
    response = Pigeon.HTTP2.establish_connection(sock)

    {:noreply, %{state | apns_socket: sock}}
  end
end
