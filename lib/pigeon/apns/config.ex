defmodule Pigeon.APNS.Config do
  @moduledoc false

  defstruct cert: nil,
            key: nil,
            port: 443,
            ping_period: 600_000,
            uri: nil

  @typedoc ~S"""
  Certificate APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.Config{
        cert: "certificate_content",
        key: "key_content",
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000
      }
  """
  @type t :: %__MODULE__{
          cert: binary | nil,
          key: binary | nil,
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer
        }

  @typedoc ~S"""
  Options for configuring certificate APNS connections.

  ## Configuration Options
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
  Returns a new `APNS.Config` with given `opts`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   mode: :prod,
      ...>   cert: File.read!("test/support/FakeAPNSCert.pem"),
      ...>   key: File.read!("test/support/FakeAPNSKey.pem"),
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{
        cert: "test/support/FakeAPNSCert.pem" |> File.read!() |> Pigeon.APNS.Config.decode_pem(),
        key: "test/support/FakeAPNSKey.pem" |> File.read!() |> Pigeon.APNS.Config.decode_pem(),
        ping_period: 300000, 
        port: 2197,
        uri: "api.push.apple.com"
      }
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      cert: opts |> Keyword.get(:cert) |> decode_pem(),
      key: opts |> Keyword.get(:key) |> decode_pem(),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      port: Keyword.get(opts, :port, 443),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode]))
    }
  end

  @doc false
  def decode_pem(bin) when is_binary(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      [{:PrivateKeyInfo, key, _}] -> {:PrivateKeyInfo, key}
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

  # Configurable Callbacks

  @spec connect(any) :: {:ok, Mint.HTTP2.t()} | {:error, Exception.t()}
  def connect(%{uri: uri, port: port} = config) do
    options = connect_socket_options(config)

    client_settings = [
      initial_window_size: round(:math.pow(2, 31) - 1),
      max_frame_size: round(:math.pow(2, 24) - 1)
    ]

    Mint.HTTP2.connect(:https, uri, port,
      client_settings: client_settings,
      transport_opts: options
    )
  end

  defdelegate push_headers(config, notification, opts), to: Shared

  defdelegate push_payload(config, notification, opts), to: Shared

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

  @spec connect_socket_options(Config.t()) :: Keyword.t()
  def connect_socket_options(%{cert: cert, key: key}) do
    [
      {:cert, cert},
      {:key, key},
      {:password, ~c""},
      {:packet, 0},
      {:reuseaddr, true}
    ]
  end
end
