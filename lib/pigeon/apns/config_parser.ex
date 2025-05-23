defmodule Pigeon.APNS.ConfigParser do
  @moduledoc false

  alias Pigeon.APNS.{Config, JWTConfig}

  @type config_opts :: [
          name: atom() | nil,
          mode: :dev | :prod | nil,
          cert: binary() | {atom(), binary()},
          key: binary() | {atom(), binary()},
          reconnect: boolean(),
          ping_period: pos_integer(),
          port: pos_integer(),
          uri: String.t(),
          key_identifier: binary() | nil,
          team_id: binary() | nil
        ]

  @type config :: Config.t() | JWTConfig.t()

  @apns_development_api_uri "api.development.push.apple.com"
  @apns_production_api_uri "api.push.apple.com"

  @filtered "[FILTERED]"

  @spec parse(atom() | config_opts()) :: config()
  def parse(opts) when is_list(opts) do
    case config_type(Enum.into(opts, %{})) do
      :error ->
        raise Pigeon.ConfigError,
          reason: "configuration is invalid",
          config: opts

      {:ok, type} ->
        type.new(opts)
    end
  end

  @spec config_type(any()) :: {:ok, module()} | :error
  defp config_type(%{cert: _cert, key_identifier: _key_id}), do: :error
  defp config_type(%{cert: _cert}), do: {:ok, Config}
  defp config_type(%{key_identifier: _jwt_key}), do: {:ok, JWTConfig}
  defp config_type(_else), do: :error

  @doc false
  @spec redact(map()) :: map()
  def redact(config) when is_map(config) do
    [:cert, :key, :keyfile]
    |> Enum.reduce(config, fn key, acc ->
      case Map.get(acc, key) do
        bin when is_binary(bin) -> Map.put(acc, key, @filtered)
        {:RSAPrivateKey, _bin} -> Map.put(acc, key, @filtered)
        _ -> acc
      end
    end)
  end

  @spec uri_for_mode(atom()) :: String.t() | nil
  def uri_for_mode(:dev), do: @apns_development_api_uri
  def uri_for_mode(:prod), do: @apns_production_api_uri
  def uri_for_mode(_else), do: nil
end
