defmodule Pigeon.APNS.Shared do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.APNS.{Config, Error, JWTConfig, Notification}

  @type config :: Config.t() | JWTConfig.t()

  @type headers :: [{String.t(), String.t()}]
  @type opts :: Keyword.t()

  @apns_id "apns-id"
  @apns_topic "apns-topic"
  @apns_priority "apns-priority"
  @apns_push_type "apns-push-type"
  @apns_expiration "apns-expiration"
  @apns_collapse_id "apns-collapse-id"

  @spec push_headers(config, Notification.t(), opts) :: headers()
  def push_headers(_config, notification, _opts) do
    json = Pigeon.json_library().encode!(notification.payload)

    [
      {":method", "POST"},
      {":path", "/3/device/#{notification.device_token}"},
      {"content-length", "#{byte_size(json)}"}
    ]
    |> put_header(@apns_id, notification.id)
    |> put_header(@apns_topic, notification.topic)
    |> put_header(@apns_priority, notification.priority)
    |> put_header(@apns_push_type, notification.push_type)
    |> put_header(@apns_expiration, notification.expiration)
    |> put_header(@apns_collapse_id, notification.collapse_id)
  end

  @spec push_payload(config, Notification.t(), opts) :: iodata | no_return
  def push_payload(_config, notification, _opts) do
    Pigeon.json_library().encode!(notification.payload)
  end

  def handle_end_stream(_config, stream, notification) do
    %{headers: headers, body: body, status: status} = stream

    case status do
      200 ->
        notification
        |> Map.put(:id, get_header(headers, @apns_id))
        |> Map.put(:response, :success)
        |> Map.put(:peername, stream[:peername])
        |> process_on_response()

      _error ->
        notification
        |> Map.put(:response, Error.parse(body))
        |> Map.put(:peername, stream[:peername])
        |> process_on_response()
    end
  end

  @spec schedule_ping(any) :: no_return
  def schedule_ping(%{ping_period: ping}) do
    Process.send_after(self(), :ping, ping)
  end

  def close(_config) do
  end

  def put_header(headers, _key, nil), do: headers

  def put_header(headers, key, val) when is_binary(val) do
    headers ++ [{key, val}]
  end

  def put_header(headers, key, val) do
    put_header(headers, key, to_string(val))
  end

  def get_header(headers, key) do
    case Enum.find(headers, fn {k, _val} -> k == key end) do
      {^key, val} -> val
      nil -> nil
    end
  end

  def add_port(opts, %{port: 443}), do: opts
  def add_port(opts, %{port: port}), do: [{:port, port} | opts]
end
