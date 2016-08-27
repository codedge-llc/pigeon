defmodule Pigeon.APNS.Config do
  @moduledoc """
    Validates configuration settings that initialize APNSWorkers.
  """

  defp default_mode, do: Application.get_env(:pigeon, :apns_mode)
  defp default_cert, do: Application.get_env(:pigeon, :apns_cert)
  defp default_key, do: Application.get_env(:pigeon, :apns_key)

  def default_config do
    %{
      mode: default_mode,
      cert: cert(default_cert),
      certfile: file_path(default_cert),
      key: key(default_key),
      keyfile: file_path(default_key)
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

  def default_keys? do
    !is_nil(default_mode) && !is_nil(default_cert) && !is_nil(default_key)
  end

  def valid?(config) do
    valid_mode? = (config[:mode] == :dev || config[:mode] == :prod)
    valid_cert? = !is_nil(config[:cert] || config[:certfile])
    valid_key? = !is_nil(config[:key] || config[:keyfile])
    valid_mode? && valid_cert? && valid_key?
  end
end
