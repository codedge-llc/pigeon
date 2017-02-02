defmodule Pigeon.APNS.Config do
  @moduledoc """
    Validates configuration settings that initialize APNSWorkers.
  """

  def default_name, do: :default

  def config(name) do
    config = Application.get_env(:pigeon, :apns)[name]
    config(name, config)
  end

  def config(name, config) do
    %{
      production_endpoint: config[:production_endpoint] ||  "api.push.apple.com",
      development_endpoint: config[:development_endpoint] ||  "api.development.push.apple.com",
      port: config[:port] ||  443,
      name: name,
      mode: config[:mode],
      cert: cert(config[:cert]),
      certfile: file_path(config[:cert]),
      key: key(config[:key]),
      keyfile: file_path(config[:key]),
      use_2197: config[:use_2197] || false
    }
  end

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
