defmodule Pigeon.APNS.Config do
  @moduledoc ~S"""
  Configuration for APNS Workers using certificates.
  """

  defstruct cert: nil,
            key: nil,
            name: nil,
            port: 443,
            ping_period: 600_000,
            uri: nil

  @typedoc ~S"""
  Certificate APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.Config{
        name: :apns_default,
        cert: "certificate_content",
        key: "key_content",
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000
      }
  """
  @type t :: %__MODULE__{
          name: atom | nil,
          cert: binary | nil,
          key: binary | nil,
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer
        }

  @typedoc ~S"""
  Options for configuring certificate APNS connections.

  ## Configuration Options
  - `:name` - Registered worker name.
  - `:mode` - If set to `:dev` or `:prod`, will set the appropriate `:uri`
  - `:cert` - Push certificate. Must be the full-text string of the file contents.
  - `:key` - Push private key. Must be the full-text string of the file contents.
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
          cert: binary,
          key: binary,
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
      ...>   cert: File.read!("test/support/FakeAPNSCert.pem"),
      ...>   key: File.read!("test/support/FakeAPNSKey.pem"),
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{
        cert: "test/support/FakeAPNSCert.pem" |> File.read!() |> Pigeon.APNS.Config.decode_pem(),
        key: "test/support/FakeAPNSKey.pem" |> File.read!() |> Pigeon.APNS.Config.decode_pem(),
        name: :test,
        ping_period: 300000, 
        port: 2197,
        uri: "api.push.apple.com"
      }

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> %{config | cert: nil, key: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com",
      ...> name: :apns_default, ping_period: 600_000, port: 443}, config
      true
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: Keyword.get(opts, :name),
      cert: opts |> Keyword.get(:cert) |> decode_pem(),
      key: opts |> Keyword.get(:key) |> decode_pem(),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      port: Keyword.get(opts, :port, 443),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode]))
    }
  end

  def new(name) when is_atom(name), do: ConfigParser.parse(name)

  @doc false
  def decode_pem(bin) when is_binary(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> {:error, {:invalid, bin}}
    end
  end

  def decode_pem(other), do: {:error, {:invalid, other}}
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  alias Pigeon.APNS.{
    Config,
    ConfigParser,
    Shared
  }

  @type sock :: {:sslsocket, any, pid | {any, any}}
  @type socket_opts :: maybe_improper_list(atom, integer | boolean)

  # Configurable Callbacks

  defdelegate worker_name(any), to: Shared

  defdelegate max_demand(any), to: Shared

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(%{uri: uri} = config) do
    uri = to_charlist(uri)
    options = connect_socket_options(config)

    Pigeon.Http2.Client.default().connect(uri, :https, options)
  end

  defdelegate push_headers(config, notification, opts), to: Shared

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  @spec validate!(Config.t()) :: :ok | no_return
  def validate!(%Config{cert: {:error, _}} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid certificate",
      config: ConfigParser.redact(config)
  end

  def validate!(%Config{key: {:error, _}} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid key",
      config: ConfigParser.redact(config)
  end

  def validate!(%Config{}) do
    :ok
  end

  @spec connect_socket_options(Config.t()) :: socket_opts
  def connect_socket_options(%{cert: cert, key: key} = config) do
    [
      {:cert, cert},
      {:key, key},
      {:password, ''},
      {:packet, 0},
      {:reuseaddr, true},
      {:active, true},
      :binary
    ]
    |> Shared.add_port(config)
  end
end
