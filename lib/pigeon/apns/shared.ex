defmodule Pigeon.APNS.Shared do
  @moduledoc false

  @type headers :: [{binary(), any()}]

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Notification, Error}

  @type sock :: {:sslsocket, any, pid | {any, any}}

  @spec worker_name(any) :: atom | nil
  def worker_name(%{name: name}), do: name

  @spec max_demand(any) :: non_neg_integer
  def max_demand(_config), do: 1_000

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(%{uri: uri} = config) do
    uri = to_charlist(uri)

    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)

      error ->
        error
    end
  end

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

  @spec push_payload(Config.t(), Notification.t(), Keyword.t()) ::
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

  def cert_option(%{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%{key: nil, keyfile: file}), do: {:keyfile, file}

  def add_port(opts, %{port: 443}), do: opts
  def add_port(opts, %{port: port}), do: [{:port, port} | opts]

  def connect_socket_options(%_{jwt_key: nil, cert: nil, certfile: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(%_{jwt_key: nil, key: nil, keyfile: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(%_{jwt_key: jwt_key} = config)
      when not is_nil(jwt_key) do
    options =
      [
        {:packet, 0},
        {:reuseaddr, true},
        {:active, true},
        :binary
      ]
      |> add_port(config)

    {:ok, options}
  end

  def connect_socket_options(config) do
    options =
      [
        cert_option(config),
        key_option(config),
        {:password, ''},
        {:packet, 0},
        {:reuseaddr, true},
        {:active, true},
        :binary
      ]
      |> add_port(config)

    {:ok, options}
  end
end
