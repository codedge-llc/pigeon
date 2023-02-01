defmodule Pigeon.APNS.JWTConfig do
  @moduledoc false

  defstruct key: nil,
            key_identifier: nil,
            ping_period: 600_000,
            port: 443,
            team_id: nil,
            uri: nil

  alias Pigeon.APNS.ConfigParser

  @type headers :: [{binary, binary}]

  @typedoc ~S"""
  JWT APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.JWTConfig{
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000,
        key: nil,
        key_identifier: "ABC1234567",
        team_id: "DEF1234567"
      }
  """
  @type t :: %__MODULE__{
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer,
          key: binary | nil | {:error, term},
          key_identifier: binary | nil,
          team_id: binary | nil
        }

  @typedoc ~S"""
  Options for configuring JWT APNS connections.

  ## Configuration Options
  - `:mode` - If set to `:dev` or `:prod`, will set the appropriate `:uri`
  - `:key` - JWT private key. Must be the full-text string of the file contents.
  - `:key_identifier` - A 10-character key identifier (kid) key, obtained from
    your developer account
  - `:team_id` - Your 10-character Team ID, obtained from your developer account
  - `:uri` - Push server uri. If set, overrides uri defined by `:mode`.
    Useful for test environments.
  - `:port` - Push server port. Can be any value, but APNS only accepts
    `443` and `2197`
  - `:ping_period` - Interval between server pings. Necessary to keep long
    running APNS connections alive. Defaults to 10 minutes.
  """
  @type config_opts :: [
          mode: :dev | :prod | nil,
          key: binary | {atom, binary},
          key_identifier: binary | nil,
          team_id: binary | nil,
          ping_period: pos_integer,
          port: pos_integer,
          uri: binary
        ]

  @doc ~S"""
  Returns a new `APNS.JWTConfig` with given `opts`.

  ## Examples

      iex> Pigeon.APNS.JWTConfig.new(
      ...>   mode: :prod,
      ...>   key: File.read!("test/support/FakeAPNSAuthKey.p8"),
      ...>   key_identifier: "ABC1234567",
      ...>   team_id: "DEF1234567",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.JWTConfig{
        uri: "api.push.apple.com",
        team_id: "DEF1234567",
        key_identifier: "ABC1234567",
        key: File.read!("test/support/FakeAPNSAuthKey.p8"),
        ping_period: 300000,
        port: 2197
      }
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      key: decode_key(opts[:key]),
      key_identifier: Keyword.get(opts, :key_identifier),
      team_id: Keyword.get(opts, :team_id)
    }
  end

  defp decode_key(bin) when is_binary(bin) do
    case :public_key.pem_decode(bin) do
      [] -> {:error, {:invalid, bin}}
      [{_, _, _}] -> bin
    end
  end

  defp decode_key(other), do: {:error, {:invalid, other}}
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.JWTConfig do
  @moduledoc false

  alias Pigeon.APNS.{
    ConfigParser,
    JWTConfig,
    Notification,
    Shared
  }

  @type headers :: [{binary, binary}]
  @type sock :: {:sslsocket, any, pid | {any, any}}
  @type socket_opts :: maybe_improper_list(atom, integer | boolean)

  # Configurable Callbacks

  @spec connect(any) :: {:ok, sock} | {:error, binary}
  def connect(%{uri: uri} = config) do
    uri = to_charlist(uri)

    options = connect_socket_options(config)
    Pigeon.Http2.Client.default().connect(uri, :https, options)
  end

  @spec push_headers(JWTConfig.t(), Notification.t(), Keyword.t()) ::
          headers | no_return
  def push_headers(%JWTConfig{} = config, notification, opts) do
    config
    |> Shared.push_headers(notification, opts)
    |> put_bearer_token(config)
  end

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification), to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  @spec validate!(JWTConfig.t()) :: :ok | no_return
  def validate!(%JWTConfig{team_id: nil} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid team_id",
      config: ConfigParser.redact(config)
  end

  def validate!(%JWTConfig{key_identifier: nil} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid key_identifier",
      config: ConfigParser.redact(config)
  end

  def validate!(%JWTConfig{key: {:error, _}} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid key",
      config: ConfigParser.redact(config)
  end

  def validate!(%JWTConfig{}) do
    :ok
  end

  @spec connect_socket_options(JWTConfig.t()) :: socket_opts
  def connect_socket_options(%{key: _jwt_key} = config) do
    [
      {:packet, 0},
      {:reuseaddr, true},
      {:active, true},
      :binary
    ]
    |> Shared.add_port(config)
  end

  @spec put_bearer_token(headers, JWTConfig.t()) :: headers
  defp put_bearer_token(headers, %JWTConfig{} = config) when is_list(headers) do
    token = Pigeon.APNS.Token.get(config)

    [{"authorization", "bearer " <> token} | headers]
  end
end
