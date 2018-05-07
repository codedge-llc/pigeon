defmodule Pigeon.APNS.ConfigParser do
  alias Pigeon.APNS.{Config, JWTConfig}

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
  - `:jwt_key` - 
  - `:jwt_key_identifier` - 
  - `:jwt_team_id` - 
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

  @type config :: Config.t() | JWTConfig.t()

  @apns_production_api_uri "api.push.apple.com"
  @apns_development_api_uri "api.development.push.apple.com"

  @spec parse(atom | config_opts) :: config | {:error, :invalid_config}
  def parse(opts) when is_list(opts) do
    case config_type(Enum.into(opts, %{})) do
      nil -> {:error, :invalid_config}
      type -> type.new(opts)
    end
  end

  def parse(name) when is_atom(name) do
    Application.get_env(:pigeon, :apns)[name]
    |> Enum.to_list()
    |> Keyword.put(:name, name)
    |> parse()
  end

  defp config_type(%{cert: cert, jwt_key: jwt_key})
       when not is_nil(cert) and not is_nil(jwt_key),
       do: :error

  defp config_type(%{cert: _cert, jwt_key: nil}), do: Config
  defp config_type(%{cert: cert}) when not is_nil(cert), do: Config
  defp config_type(%{cert: nil, jwt_key: _jwt_key}), do: JWTConfig
  defp config_type(%{jwt_key: jwt_key}) when not is_nil(jwt_key), do: JWTConfig
  defp config_type(_), do: :error

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

  def uri_for_mode(:dev), do: @apns_development_api_uri
  def uri_for_mode(:prod), do: @apns_production_api_uri
  def uri_for_mode(_else), do: nil
end
