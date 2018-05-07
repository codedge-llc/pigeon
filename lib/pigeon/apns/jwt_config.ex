defmodule Pigeon.APNS.JWTConfig do
  @moduledoc """
  Configuration for APNS Workers
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
  APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.Config{
        name: :apns_default,
        reconnect: true,
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000,
        jwt_key: {:app, "key.p8"},
        jwt_keyfile: "key.p8",
        jwt_key_identifier: "ABC1234567",
        jwt_team_id: "DEF1234567"
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
          key_identifier: nil,
          team_id: nil
        }

  @doc ~S"""
  Returns a new `APNS.Config` or `APNS.JWTConfig` with given `opts` or name.

  If given an atom, returns the config specified in your `mix.exs`.

  ## Examples

      iex> Pigeon.APNS.JWTConfig.new(
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
      iex> %{config | certfile: nil, keyfile: nil, jwt_key: nil, jwt_keyfile: nil, jwt_key_identifier: nil, jwt_team_id: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com", name: :apns_default, ping_period: 600_000, port: 443, reconnect: false}, config
      true
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      key: opts[:jwt_key],
      keyfile: ConfigParser.file_path(opts[:jwt_key]),
      key_identifier: Keyword.get(opts, :jwt_key_identifier),
      team_id: Keyword.get(opts, :jwt_team_id)
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
    {timestamp, saved_token} = Pigeon.APNS.Token.get(config.name)
    now = :os.system_time(:seconds)

    token =
      case now - timestamp do
        age when age < @token_max_age -> saved_token
        _ -> generate_apns_jwt(config)
      end

    [{"authorization", "bearer " <> token} | headers]
  end

  @spec generate_apns_jwt(JWTConfig.t()) :: String.t()
  defp generate_apns_jwt(config) do
    import Joken

    key = get_token_key(config)

    now = :os.system_time(:seconds)

    token =
      token()
      |> with_claims(%{"iss" => config.jwt_team_id, "iat" => now})
      |> with_header_arg("alg", "ES256")
      |> with_header_arg("typ", "JWT")
      |> with_header_arg("kid", config.key_identifier)
      |> sign(es256(key))
      |> get_compact

    :ok = Pigeon.APNS.Token.update(config.name, now, token)

    token
  end

  @spec get_token_key(JWTConfig.t()) :: JOSE.JWK.t()
  defp get_token_key(%JWTConfig{keyfile: nil} = config) do
    JOSE.JWK.from_pem(config.jwt_key)
  end

  defp get_token_key(%JWTConfig{keyfile: file}) do
    JOSE.JWK.from_pem_file(file)
  end
end
