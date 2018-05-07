defmodule Pigeon.APNS.Config do
  @moduledoc """
  Configuration for APNS Workers
  """

  alias Pigeon.APNS.{CertConfig, JWTConfig}

  @apns_production_api_uri "api.push.apple.com"
  @apns_development_api_uri "api.development.push.apple.com"

  @type t :: CertConfig.t() | JWTConfig.t()

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

  @doc false
  def default_name, do: :apns_default

  @doc ~S"""
  Returns a new `APNS.CertConfig` or `APNS.JWTConfig` with given `opts` or name.

  If given an atom, returns the config specified in your `mix.exs`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   cert: "test_cert.pem",
      ...>   key: "test_key.pem",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.CertConfig{uri: "api.push.apple.com", name: :test,
      ping_period: 300000, port: 2197, reconnect: false}

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> %{config | certfile: nil, keyfile: nil, jwt_key: nil, jwt_keyfile: nil, jwt_key_identifier: nil, jwt_team_id: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com", name: :apns_default, ping_period: 600_000, port: 443, reconnect: false}, config
      true
  """
  @spec new(atom | config_opts) :: CertConfig.t() | JWTConfig.t()
  def new(opts) when is_list(opts) do
    case check_config(Enum.into(opts, %{})) do
      :cert -> new_cert_config(opts)
      :jwt -> new_jwt_config(opts)
    end
  end

  def new(name) when is_atom(name) do
    Application.get_env(:pigeon, :apns)[name]
    |> Enum.to_list()
    |> Keyword.put(:name, name)
    |> new()
  end

  defp new_cert_config(opts) do
    %CertConfig{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      cert: cert(opts[:cert]),
      certfile: file_path(opts[:cert]),
      key: key(opts[:key]),
      keyfile: file_path(opts[:key]),
      uri: Keyword.get(opts, :uri, uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000)
    }
  end

  defp new_jwt_config(opts) do
    %JWTConfig{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      uri: Keyword.get(opts, :uri, uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000),
      jwt_key: opts[:jwt_key],
      jwt_keyfile: file_path(opts[:jwt_key]),
      jwt_key_identifier: Keyword.get(opts, :jwt_key_identifier),
      jwt_team_id: Keyword.get(opts, :jwt_team_id)
    }
  end

  defp check_config(%{cert: cert, jwt_key: jwt_key})
       when not is_nil(cert) and not is_nil(jwt_key) do
    raise Pigeon.ConfigError,
      message:
        "Invalid Configuration. :cert and :jwt_key can't both be configured."
  end

  defp check_config(%{cert: _cert, jwt_key: nil}), do: :cert
  defp check_config(%{cert: cert}) when not is_nil(cert), do: :cert
  defp check_config(%{cert: nil, jwt_key: _jwt_key}), do: :jwt
  defp check_config(%{jwt_key: jwt_key}) when not is_nil(jwt_key), do: :jwt

  defp check_config(_) do
    raise Pigeon.ConfigError,
      message:
        "Invalid Configuration. Either :cert or :jwt_key must be configured."
  end

  defp uri_for_mode(:dev), do: @apns_development_api_uri
  defp uri_for_mode(:prod), do: @apns_production_api_uri
  defp uri_for_mode(_else), do: nil

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
end
