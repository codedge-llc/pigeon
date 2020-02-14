defmodule Pigeon.APNS.Config do
  @moduledoc ~S"""
  Configuration for APNS Workers using certificates.
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
  Certificate APNS configuration struct

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
  Options for configuring certificate APNS connections.

  ## Configuration Options
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

  ## Deprecated Options
  - `:reconnect` - No longer used as of `v1.2.0`.
  """
  @type config_opts :: [
          name: atom | nil,
          mode: :dev | :prod | nil,
          cert: binary | {atom, binary},
          key: binary | {atom, binary},
          reconnect: boolean,
          ping_period: pos_integer,
          port: pos_integer,
          uri: binary,
          jwt_key: binary | {atom, binary},
          jwt_key_identifier: binary | nil,
          jwt_team_id: binary | nil
        ]

  alias Pigeon.APNS.ConfigParser

  @doc false
  def default_name, do: :apns_default

  @doc ~S"""
  Returns a new `APNS.Config` with given `opts` or name.

  If given an atom, returns the config specified in your `config.exs`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   cert: "test/support/test_cert.pem-mock",
      ...>   key: "test/support/test_key.pem-mock",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{uri: "api.push.apple.com", name: :test,
      certfile: Path.expand("test/support/test_cert.pem-mock"),
      keyfile: Path.expand("test/support/test_key.pem-mock"),
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
      cert: cert(opts[:cert]),
      certfile: ConfigParser.file_path(opts[:cert]),
      key: key(opts[:key]),
      keyfile: ConfigParser.file_path(opts[:key]),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000)
    }
    |> ConfigParser.strip_errors(:cert, :certfile)
    |> ConfigParser.strip_errors(:key, :keyfile)
  end

  def new(name) when is_atom(name), do: ConfigParser.parse(name)

  defp cert(bin) when is_binary(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      _ -> {:error, {:invalid, bin}}
    end
  end

  defp cert(other), do: {:error, {:invalid, other}}

  defp key(bin) when is_binary(bin) do
    case :public_key.pem_decode(bin) do
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> {:error, {:invalid, bin}}
    end
  end

  defp key(other), do: {:error, {:invalid, other}}
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  alias Pigeon.APNS.Shared

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  defdelegate worker_name(any), to: Shared

  defdelegate max_demand(any), to: Shared

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

  defdelegate push_headers(config, notification, opts), to: Shared

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  def validate!(config) do
    case config do
      %{cert: {:error, _}, certfile: {:error, _}} ->
        raise Pigeon.ConfigError,
          reason: "attempted to start without valid certificate",
          config: redact(config)

      %{key: {:error, _}, keyfile: {:error, _}} ->
        raise Pigeon.ConfigError,
          reason: "attempted to start without valid key",
          config: redact(config)

      _ ->
        :ok
    end
  end

  defp redact(config) do
    [:cert, :key]
    |> Enum.reduce(config, fn key, acc ->
      case Map.get(acc, key) do
        bin when is_binary(bin) -> Map.put(acc, key, "[FILTERED]")
        {:RSAPrivateKey, _bin} -> Map.put(acc, key, "[FILTERED]")
        _ -> acc
      end
    end)
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
      |> Shared.add_port(config)

    {:ok, options}
  end

  def cert_option(%{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%{key: nil, keyfile: file}), do: {:keyfile, file}
end
