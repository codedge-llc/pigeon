defmodule Pigeon.APNS.Config do
  @moduledoc """
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

  @typedoc ~S"""
  Options for configuring APNS connections.

  - `:name` - Registered worker name.
  - `:mode` - If set to `:dev` or `:prod`, will set the appropriate `:uri`
  - `:cert` - Push certificate. Can be one of three options:
    - Static file path
    - Full-text string of the file contents (useful for environment variables)
    - `{:my_app, "certs/cert.pem"}` (indicates path relative to the `priv`
      folder of the given application)
  - `:key` - Push private key. Same as `:cert`
  - `:uri` - Push server uri. If set, overrides uri defined by `:mode`.
    Useful for test environments.
  - `:port` - Push server port. Can be any value, but APNS only accepts
    `443` and `2197`
  - `:ping_period` - Interval between server pings. Necessary to keep long
    running APNS connections alive. Defaults to 10 minutes.
  """
  @type config_opts :: [
    name: atom | nil,
    mode: :dev | :prod | nil,
    cert: binary | {atom, binary},
    key: binary | {atom, binary},
    reconnect: boolean,
    ping_period: pos_integer,
    port: pos_integer,
    uri: binary
  ]

  @apns_production_api_uri "api.push.apple.com"
  @apns_development_api_uri "api.development.push.apple.com"

  @doc false
  def default_name, do: :apns_default

  @doc ~S"""
  Returns a new `APNS.Config` with given `opts` or name.

  If given an atom, returns the config specified in your `mix.exs`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   cert: "test_cert.pem",
      ...>   key: "test_key.pem",
      ...>   reconnect: false,
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{uri: "api.push.apple.com", name: :test,
      ping_period: 300000, port: 2197, reconnect: false}

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> %{config | certfile: nil, keyfile: nil} # Hide for testing
      %Pigeon.APNS.Config{uri: "api.development.push.apple.com",
      name: :apns_default, ping_period: 600_000, port: 443, reconnect: false}
  """
  @spec new(atom | config_opts) :: t
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      cert: cert(opts[:cert]),
      certfile: file_path(opts[:cert]),
      key: key(opts[:key]),
      keyfile: file_path(opts[:key]),
      uri: Keyword.get(opts, :uri, uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000)
    }
  end
  def new(name) when is_atom(name) do
    Application.get_env(:pigeon, :apns)[name]
    |> Enum.to_list
    |> Keyword.put(:name, name)
    |> new()
  end

  defp uri_for_mode(:dev), do: @apns_development_api_uri
  defp uri_for_mode(:prod), do: @apns_production_api_uri
  defp uri_for_mode(_else), do: nil

  @doc false
  def file_path(nil), do: nil
  def file_path(path) when is_binary(path) do
    if :filelib.is_file(path), do: Path.expand(path), else: nil
  end
  def file_path({app_name, path}) when is_atom(app_name),
    do: Path.expand(path, :code.priv_dir(app_name))

  @doc false
  def cert({_app_name, _path}), do: nil
  def cert(nil), do: nil
  def cert(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      _ -> nil
    end
  end

  @doc false
  def key({_app_name, _path}), do: nil
  def key(nil), do: nil
  def key(bin) do
    case :public_key.pem_decode(bin) do
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> nil
    end
  end
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Error}

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  @spec worker_name(any) :: atom | nil
  def worker_name(%Config{name: name}), do: name

  @spec max_demand(any) :: non_neg_integer
  def max_demand(_config), do: 1_000

  @spec connect(any) :: {:ok, sock} | {:error, String.t}
  def connect(%Config{uri: uri} = config) do
    uri = to_charlist(uri)
    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)
      error -> error
    end
  end

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

  def push_payload(_config, notification, _opts) do
    Poison.encode!(notification.payload)
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

  def handle_end_stream(_config,
                        %{headers: headers, body: body, status: status},
                        notification,
                        on_response) do
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

  def get_apns_id(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == "apns-id" end) do
      {"apns-id", id} -> id
      nil -> nil
    end
  end

  @spec schedule_ping(any) :: no_return
  def schedule_ping(%Config{ping_period: ping}) do
    Process.send_after(self(), :ping, ping)
  end

  def close(_config) do
  end

  # Everything Else

  def connect_socket_options(%Config{cert: nil, certfile: nil}) do
    {:error, :invalid_config}
  end
  def connect_socket_options(%Config{key: nil, keyfile: nil}) do
    {:error, :invalid_config}
  end
  def connect_socket_options(config) do
    options = [
      cert_option(config),
      key_option(config),
      {:password, ''},
      {:packet, 0},
      {:reuseaddr, true},
      {:active, true},
      {:reconnect, config.reconnect},
      :binary
    ]
    |> add_port(config)

    {:ok, options}
  end

  def cert_option(%Config{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%Config{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%Config{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%Config{key: nil, keyfile: file}), do: {:keyfile, file}

  defp add_port(opts, %Config{port: 443}), do: opts
  defp add_port(opts, %Config{port: port}), do: [{:port, port} | opts]
end
