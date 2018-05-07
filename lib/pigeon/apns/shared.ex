defmodule Pigeon.APNS.Shared do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Error, JWTConfig, Notification}

  @type config :: Config.t() | JWTConfig.t()

  @type headers :: [{binary(), any()}]

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
    |> put_apns_id(notification)
    |> put_apns_topic(notification)
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
        n = %{notification | id: get_apns_id(headers), response: :success}
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

  def put_apns_id(headers, notification) do
    case notification.id do
      nil -> headers
      id -> headers ++ [{"apns-id", id}]
    end
  end

  def put_apns_topic(headers, notification) do
    case notification.topic do
      nil -> headers
      topic -> headers ++ [{"apns-topic", topic}]
    end
  end

  def get_apns_id(headers) do
    case Enum.find(headers, fn {key, _val} -> key == "apns-id" end) do
      {"apns-id", id} -> id
      nil -> nil
    end
  end

  def add_port(opts, %{port: 443}), do: opts
  def add_port(opts, %{port: port}), do: [{:port, port} | opts]
end
