defmodule Pigeon.APNSWorker do
  @moduledoc """
    Handles all APNS request and response parsing over an HTTP2 connection.
  """
  use GenServer
  require Logger

  defp apns_production_api_uri, do: "api.push.apple.com"
  defp apns_development_api_uri, do: "api.development.push.apple.com"

  def push_uri(mode) do
    case mode do
      :dev -> apns_development_api_uri
      :prod -> apns_production_api_uri
    end
  end

  def start_link(name, config) do
    GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  end

  def stop, do: :gen_server.cast(self, :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    mode = config[:mode]
    case connect_socket(mode, config) do
      {:ok, socket} ->
        {:ok, %{
          apns_socket: socket,
          mode: mode,
          config: config,
          stream_id: 1,
          queue: %{}
        }}
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
    uri = mode |> push_uri |> to_char_list
    options = [cert,
               key,
               {:password, ''},
               {:packet, 0},
               {:reuseaddr, false},
               {:active, true},
               :binary]
    options =
      case Application.get_env(:pigeon, :apns_2197) do
        true -> options ++ [{:port, 2197}]
        _ -> options
      end

    case :h2_client.start_link(:https, uri, options) do
      {:ok, socket} -> {:ok, socket}
      {:error, _} -> connect_socket(mode, cert, key, tries + 1)
    end
  end

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, :apns, notification}, state) do
    send_push(state, notification, nil)
  end

  def handle_cast({:push, :apns, notification, on_response}, state) do
    send_push(state, notification, on_response)
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

    :h2_client.send_request(socket, req_headers, json)
    new_q = Map.put(queue, "#{stream_id}", {notification, on_response})
    new_stream_id = stream_id + 2
    { :noreply, %{state | stream_id: new_stream_id, queue: new_q } }
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

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
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

  def handle_info({:END_STREAM, stream}, state) do
    %{apns_socket: socket, queue: queue} = state
    queue |> inspect |> Logger.debug

    {:ok, {headers, body}} = :h2_client.get_response(socket, stream)
    {notification, on_response} = queue["#{stream}"]

    case get_status(headers) do
      "200" ->
        notification = %{notification | id: get_apns_id(headers)}
        unless on_response == nil do on_response.({:ok, notification}) end
        new_queue = Map.delete(queue, "#{stream}")
        {:noreply, %{state | queue: new_queue}}
      nil ->
        {:noreply, state}
      _error ->
        reason = parse_error(body)
        log_error(reason, notification)
        unless on_response == nil do on_response.({:error, reason, notification}) end
        new_queue = Map.delete(queue, "#{stream}")
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
