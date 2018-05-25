defmodule Pigeon.APNS.JWTConfig do
  @moduledoc """
  Configuration for APNS Workers using JWT.
  """

  defstruct name: nil,
            reconnect: true,
            uri: nil,
            port: 443,
            ping_period: 600_000,
            key: nil,
            keyfile: nil,
            key_identifier: nil,
            team_id: nil

  alias Pigeon.APNS.{Config, ConfigParser}

  @typedoc ~S"""
  JWT APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.JWTConfig{
        name: :apns_default,
        reconnect: true,
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000,
        key: nil,
        keyfile: "key.p8",
        key_identifier: "ABC1234567",
        team_id: "DEF1234567"
      }
  """
  @type t :: %__MODULE__{
          name: atom | nil,
          reconnect: boolean,
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer,
          key: binary | nil,
          keyfile: binary | nil,
          key_identifier: binary | nil,
          team_id: binary | nil
        }

  @typedoc ~S"""
  Options for configuring JWT APNS connections.

  ## Configuration Options
  - `:name` - Registered worker name.
  - `:mode` - If set to `:dev` or `:prod`, will set the appropriate `:uri`
  - `:key` - JWT private key. Can be one of three options:
    - Static file path
    - Full-text string of the file contents (useful for environment variables)
    - `{:my_app, "keys/private_key.p8"}` (indicates path relative to the `priv`
      folder of the given application)
  - `:key_identifier` - A 10-character key identifier (kid) key, obtained from
    your developer account
  - `:team_id` - Your 10-character Team ID, obtained from your developer account
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
          key: binary | {atom, binary},
          key_identifier: binary | nil,
          team_id: binary | nil,
          reconnect: boolean,
          ping_period: pos_integer,
          port: pos_integer,
          uri: binary
        ]

  @doc ~S"""
  Returns a new `APNS.JWTConfig` with given `opts` or name.

  If given an atom, returns the config specified in your `config.exs`.

  ## Examples

      iex> Pigeon.APNS.JWTConfig.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   key: "key.p8",
      ...>   key_identifier: "ABC1234567",
      ...>   team_id: "DEF1234567",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.JWTConfig{uri: "api.push.apple.com", name: :test,
      team_id: "DEF1234567", key_identifier: "ABC1234567", key: "key.p8",
      ping_period: 300000, port: 2197, reconnect: false}

      iex> config = Pigeon.APNS.JWTConfig.new(:apns_jwt_static)
      iex> %{config | key: nil, key_identifier: nil, team_id: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com", name: :apns_jwt_static,
      ...> ping_period: 600_000, port: 443, reconnect: false}, config
      true
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      key: opts[:key],
      keyfile: ConfigParser.file_path(opts[:key]),
      key_identifier: Keyword.get(opts, :key_identifier),
      team_id: Keyword.get(opts, :team_id)
    }
  end

  def new(name) when is_atom(name), do: ConfigParser.parse(name)
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.JWTConfig do
  @moduledoc false

  alias Pigeon.APNS.{Config, JWTConfig, Notification, Shared}

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Seconds
  @token_max_age 3_590

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

  @spec push_headers(JWTConfig.t(), Notification.t(), Keyword.t()) ::
          Shared.headers()
  def push_headers(config, notification, opts) do
    config
    |> Shared.push_headers(notification, opts)
    |> put_bearer_token(config)
  end

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  def connect_socket_options(%{key: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(%{key: _jwt_key} = config) do
    options =
      [
        {:packet, 0},
        {:reuseaddr, true},
        {:active, true},
        :binary
      ]
      |> Shared.add_port(config)

    {:ok, options}
  end

  @spec put_bearer_token(Config.headers(), JWTConfig.t()) :: Config.headers()
  defp put_bearer_token(headers, %{key: nil}), do: headers

  defp put_bearer_token(headers, config) do
    token_storage_key = config.key_identifier <> ":" <> config.team_id
    {timestamp, saved_token} = Pigeon.APNS.Token.get(token_storage_key)
    now = :os.system_time(:seconds)

    token =
      case now - timestamp do
        age when age < @token_max_age -> saved_token
        _ -> generate_apns_jwt(config, token_storage_key)
      end

    [{"authorization", "bearer " <> token} | headers]
  end

  @spec generate_apns_jwt(JWTConfig.t(), String.t()) :: String.t()
  defp generate_apns_jwt(config, token_storage_key) do
    import Joken

    key = get_token_key(config)

    now = :os.system_time(:seconds)

    token =
      token()
      |> with_claims(%{"iss" => config.team_id, "iat" => now})
      |> with_header_arg("alg", "ES256")
      |> with_header_arg("typ", "JWT")
      |> with_header_arg("kid", config.key_identifier)
      |> sign(es256(key))
      |> get_compact

    :ok = Pigeon.APNS.Token.update(token_storage_key, {now, token})

    token
  end

  @spec get_token_key(JWTConfig.t()) :: JOSE.JWK.t()
  defp get_token_key(%JWTConfig{keyfile: nil} = config) do
    JOSE.JWK.from_pem(config.key)
  end

  defp get_token_key(%JWTConfig{keyfile: file}) do
    JOSE.JWK.from_pem_file(file)
  end
end
