defmodule Pigeon.APNS.Config do
  @moduledoc """
  Validates configuration settings that initialize APNS.Workers.
  """

  defstruct name: nil,
            mode: nil,
            reconnect: true,
            cert: nil,
            certfile: nil,
            key: nil,
            keyfile: nil,
            use_2197: false,
            ping_period: 600_000

  def default_name, do: :apns_default

  def config(name) do
    config = Application.get_env(:pigeon, :apns)[name]
    %{
      name: name,
      mode: mode(config[:mode]),
      reconnect: Map.get(config, :reconnect, true),
      cert: cert(config[:cert]),
      certfile: file_path(config[:cert]),
      key: key(config[:key]),
      keyfile: file_path(config[:key]),
      use_2197: config[:use_2197] || false,
      ping_period: config[:ping_period] || 600_000
    }
  end

  def mode({:system, env_var}), do: to_mode(System.get_env(env_var))
  def mode(mode), do: mode

  def to_mode(nil), do: raise "APNS.Config mode is nil"
  def to_mode("dev"), do: :dev
  def to_mode(":dev"), do: :dev
  def to_mode("prod"), do: :prod
  def to_mode(":prod"), do: :prod
  def to_mode(other), do: raise "APNS.Config mode is #{inspect(other)}"

  def file_path(nil), do: nil
  def file_path(path) when is_binary(path) do
    cond do
      :filelib.is_file(path) -> Path.expand(path)
      true -> nil
    end
  end
  def file_path({app_name, path}) when is_atom(app_name),
    do: Path.expand(path, :code.priv_dir(app_name))

  def cert({_app_name, _path}), do: nil
  def cert(nil), do: nil
  def cert(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      _ -> nil
    end
  end

  def key({_app_name, _path}), do: nil
  def key(nil), do: nil
  def key(bin) do
    case :public_key.pem_decode(bin) do
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> nil
    end
  end

  def valid?(config) do
    valid_mode? = (config[:mode] == :dev || config[:mode] == :prod)
    valid_cert? = !is_nil(config[:cert] || config[:certfile])
    valid_key? = !is_nil(config[:key] || config[:keyfile])
    valid_mode? && valid_cert? && valid_key?
  end
end
