defmodule Pigeon.APNS.JWTConfig do
  @moduledoc """
  Configuration for APNS Workers
  """

  defstruct name: nil,
            reconnect: true,
            uri: nil,
            port: 443,
            ping_period: 600_000,
            jwt_key: nil,
            jwt_keyfile: nil,
            jwt_key_identifier: nil,
            jwt_team_id: nil

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
          jwt_key: binary | nil,
          jwt_keyfile: binary | nil,
          jwt_key_identifier: nil,
          jwt_team_id: nil
        }
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.JWTConfig do
  @moduledoc false

  alias Pigeon.APNS.{JWTConfig, Notification, Shared}

  # Seconds
  @token_max_age 3_590

  # Configurable Callbacks

  defdelegate worker_name(any), to: Shared

  defdelegate max_demand(any), to: Shared

  defdelegate connect(any), to: Shared

  @spec push_headers(JWTConfig.t(), Notification.t(), Keyword.t()) ::
          Shared.headers()
  def push_headers(config, notification, opts) do
    Shared.push_headers(config, notification, opts)
    |> put_apns_jwt_authentication(config)
  end

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  @spec put_apns_jwt_authentication(Shared.headers(), JWTConfig.t()) ::
          Shared.headers()
  defp put_apns_jwt_authentication(headers, %{jwt_key: nil}), do: headers

  defp put_apns_jwt_authentication(headers, config) do
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
      |> with_header_arg("kid", config.jwt_key_identifier)
      |> sign(es256(key))
      |> get_compact

    :ok = Pigeon.APNS.Token.update(config.name, now, token)

    token
  end

  @spec get_token_key(JWTConfig.t()) :: JOSE.JWK.t()
  defp(get_token_key(%JWTConfig{jwt_keyfile: nil} = config)) do
    JOSE.JWK.from_pem(config.jwt_key)
  end

  defp get_token_key(%JWTConfig{jwt_keyfile: file}) do
    JOSE.JWK.from_pem_file(file)
  end
end
