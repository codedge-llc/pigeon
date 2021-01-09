defmodule Pigeon.APNS.ConfigParser do
  @moduledoc false

  alias Pigeon.APNS.{Config, JWTConfig}

  @type config_opts :: [
          name: atom | nil,
          mode: :dev | :prod | nil,
          cert: binary | {atom, binary},
          key: binary | {atom, binary},
          reconnect: boolean,
          ping_period: pos_integer,
          port: pos_integer,
          uri: binary,
          key_identifier: binary | nil,
          team_id: binary | nil
        ]

  @type config :: Config.t() | JWTConfig.t()

  @apns_production_api_uri "api.push.apple.com"
  @apns_development_api_uri "api.development.push.apple.com"

  @spec parse(atom | config_opts) :: config | {:error, :invalid_config}
  def parse(opts) when is_list(opts) do
    case config_type(Enum.into(opts, %{})) do
      :error -> raise Pigeon.ConfigError, reason: "configuration is invalid", config: opts
      type -> type.new(opts)
    end
  end

  def parse(name) when is_atom(name) do
    Application.get_env(:pigeon, :apns)[name]
    |> Enum.to_list()
    |> Keyword.put(:name, name)
    |> parse()
  end

  @spec config_type(any) :: module | :error
  defp config_type(%{cert: _cert, key_identifier: _key_id}), do: :error
  defp config_type(%{cert: _cert}), do: Config
  defp config_type(%{key_identifier: _jwt_key}), do: JWTConfig
  defp config_type(_else), do: :error

  @doc false
  @spec file_path(binary) :: binary | {:error, {:nofile, binary}}
  def file_path(path) when is_binary(path) do
    if :filelib.is_file(path) do
      Path.expand(path)
    else
      {:error, {:nofile, path}}
    end
  end

  def file_path({app_name, path}) when is_atom(app_name) do
    path
    |> Path.expand(:code.priv_dir(app_name))
    |> file_path()
  end

  def file_path(other), do: {:error, {:nofile, other}}

  @doc false
  def redact(config) when is_map(config) do
    [:cert, :key, :keyfile]
    |> Enum.reduce(config, fn key, acc ->
      case Map.get(acc, key) do
        bin when is_binary(bin) -> Map.put(acc, key, "[FILTERED]")
        {:RSAPrivateKey, _bin} -> Map.put(acc, key, "[FILTERED]")
        _ -> acc
      end
    end)
  end

  @spec uri_for_mode(atom) :: binary | nil
  def uri_for_mode(:dev), do: @apns_development_api_uri
  def uri_for_mode(:prod), do: @apns_production_api_uri
  def uri_for_mode(_else), do: nil
end
