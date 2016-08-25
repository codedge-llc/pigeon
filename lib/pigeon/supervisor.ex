defmodule Pigeon.Supervisor do
  @moduledoc """
    Supervises an APNSWorker, restarting as necessary.
  """
  use Supervisor
  require Logger

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: :pigeon)
  end

  def stop, do: :gen_server.cast(:pigeon, :stop)

  def init(:ok) do
    children =
      cond do
        !apns_keys? ->
          []
        valid_apns_config?(ssl_config) ->
          poolboy_config = [
            {:name, {:local, pool_name}},
            {:worker_module, Pigeon.APNSWorker},
            {:size, 5},
            {:max_overflow, 5}
          ]
          [:poolboy.child_spec(pool_name, poolboy_config, ssl_config)]
        true ->
          Logger.error "Error starting :apns_worker. Invalid mode/cert/key configuration."
          []
      end
    supervise(children, strategy: :one_for_one)
  end

  defp pool_name, do: :default

  defp config_mode, do: Application.get_env(:pigeon, :apns_mode)
  defp config_cert, do: Application.get_env(:pigeon, :apns_cert)
  defp config_key, do: Application.get_env(:pigeon, :apns_key)


  def ssl_config do
    %{
      mode: config_mode,
      cert: cert(config_cert),
      certfile: file_path(config_cert),
      key: key(config_key),
      keyfile: file_path(config_key)
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

  def apns_keys? do
    mode = Application.get_env(:pigeon, :apns_mode)
    cert = Application.get_env(:pigeon, :apns_cert)
    key = Application.get_env(:pigeon, :apns_key)
    !is_nil(mode) && !is_nil(cert) && !is_nil(key)
  end

  def valid_apns_config?(config) do
    valid_mode? = (config[:mode] == :dev || config[:mode] == :prod)
    valid_cert? = !is_nil(config[:cert] || config[:certfile])
    valid_key? = !is_nil(config[:key] || config[:keyfile])
    valid_mode? && valid_cert? && valid_key?
  end

  def push(service, notification) do
    case service do
      :apns ->
        :poolboy.transaction(pool_name, fn(worker) ->
          GenServer.cast(worker, {:push, :apns, notification})
        end)
      _ ->
        Logger.error "Unknown service #{service}"
    end
  end

  def push(service, notification, on_response) do
    case service do
      :apns ->
        :poolboy.transaction(pool_name, fn(worker) ->
          GenServer.cast(worker, {:push, :apns, notification, on_response})
        end)
      _ ->
        Logger.error "Unknown service #{service}"
    end
  end

  def handle_cast(:stop, state), do: { :noreply, state }
end
