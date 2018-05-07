defmodule Pigeon.APNS.Config do
  @moduledoc ~S"""
  Configuration for APNS Workers
  """

  defstruct name: nil,
            reconnect: true,
            cert: nil,
            certfile: nil,
            key: nil,
            keyfile: nil,
            uri: nil,
            port: 443,
            ping_period: 600_000

  @typedoc ~S"""
  APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.Config{
        name: :apns_default,
        reconnect: true,
        cert: nil,
        certfile: "cert.pem",
        key: nil,
        keyfile: "key.pem",
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000
      }
  """
  @type t :: %__MODULE__{
          name: atom | nil,
          reconnect: boolean,
          cert: binary | nil,
          certfile: binary | nil,
          key: binary | nil,
          keyfile: binary | nil,
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer
        }

  alias Pigeon.APNS.ConfigParser

  @doc false
  def default_name, do: :apns_default

  @doc ~S"""
  Returns a new `APNS.Config` or `APNS.JWTConfig` with given `opts` or name.

  If given an atom, returns the config specified in your `mix.exs`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   cert: "test_cert.pem",
      ...>   key: "test_key.pem",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{uri: "api.push.apple.com", name: :test,
      ping_period: 300000, port: 2197, reconnect: false}

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> %{config | certfile: nil, keyfile: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com",
      ...> name: :apns_default, ping_period: 600_000, port: 443}, config
      true
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      cert: ConfigParser.cert(opts[:cert]),
      certfile: ConfigParser.file_path(opts[:cert]),
      key: ConfigParser.key(opts[:key]),
      keyfile: ConfigParser.file_path(opts[:key]),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000)
    }
  end

  def new(name) when is_atom(name), do: ConfigParser.parse(name)
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Notification, Error}

  @type headers :: [{binary(), any()}]

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

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

  def connect_socket_options(%{cert: nil, certfile: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(%{key: nil, keyfile: nil}) do
    {:error, :invalid_config}
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

  def cert_option(%{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%{key: nil, keyfile: file}), do: {:keyfile, file}

  def add_port(opts, %{port: 443}), do: opts
  def add_port(opts, %{port: port}), do: [{:port, port} | opts]
end
