defmodule Pigeon.APNS.Shared do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Error, JWTConfig, Notification}

  @type config :: Config.t() | JWTConfig.t()

  @type headers :: [{binary(), any()}]

  @apns_id "apns-id"
  @apns_topic "apns-topic"
  @apns_expiration "apns-expiration"
  @apns_collapse_id "apns-collapse-id"

  @spec worker_name(any) :: atom | nil
  def worker_name(%{name: name}), do: name

  @spec max_demand(any) :: non_neg_integer
  def max_demand(_config), do: 1_000

  @spec push_headers(Config.t(), Notification.t(), Keyword.t()) :: headers()
  def push_headers(_config, notification, _opts) do
    json = Poison.encode!(notification.payload)

    [
      {":method", "POST"},
      {":path", "/3/device/#{notification.device_token}"},
      {"content-length", "#{byte_size(json)}"}
    ]
    |> put_header(@apns_id, notification.id)
    |> put_header(@apns_topic, notification.topic)
    |> put_header(@apns_expiration, notification.expiration)
    |> put_header(@apns_collapse_id, notification.collapse_id)
  end

  @spec push_payload(config, Notification.t(), Keyword.t()) ::
          iodata | no_return
  def push_payload(_config, notification, _opts) do
    Poison.encode!(notification.payload)
  end

  def handle_end_stream(_config, stream, notification, on_response) do
    %{headers: headers, body: body, status: status} = stream

    case status do
      200 ->
        n =
          notification
          |> Map.put(:id, get_header(headers, @apns_id))
          |> Map.put(:response, :success)

        process_on_response(on_response, n)

      _error ->
        reason = Error.parse(body)
        Error.log(reason, notification)
        notification = %{notification | response: reason}
        process_on_response(on_response, notification)
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
