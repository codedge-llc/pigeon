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
    children = apns_children ++ adm_children
    supervise(children, strategy: :one_for_one)
  end

  defp apns_children do
    cond do
      !apns_keys? ->
        []
      valid_apns_config?(ssl_config) ->
        [worker(Pigeon.APNSWorker, [:apns_worker, ssl_config], id: :apns_worker)]
      true ->
        Logger.error "Error starting :apns_worker. Invalid mode/cert/key configuration."
        []
    end
  end

  defp adm_children do
    cond do
      !adm_configured? ->
        []
      valid_adm_config?(adm_config) ->
        [worker(Pigeon.ADMWorker, [:adm_worker, adm_config], id: :adm_worker)]
      true ->
        Logger.error "Error starting :adm_worker. Invalid OAuth2 configuration."
        []
    end
  end

  defp config_mode, do: Application.get_env(:pigeon, :apns_mode)
  defp config_cert, do: Application.get_env(:pigeon, :apns_cert)
  defp config_key, do: Application.get_env(:pigeon, :apns_key)

  defp config_adm_client_id, do: Application.get_env(:pigeon, :adm_client_id)
  defp config_adm_client_secret, do: Application.get_env(:pigeon, :adm_client_secret)

  def ssl_config do
    %{
      mode: config_mode,
      cert: cert(config_cert),
      certfile: file_path(config_cert),
      key: key(config_key),
      keyfile: file_path(config_key)
    }
  end

  defp file_path(nil), do: nil
  defp file_path(path) when is_binary(path) do
    cond do
      :filelib.is_file(path) -> Path.expand(path)
      true -> nil
    end
  end
  defp file_path({app_name, path}) when is_atom(app_name),
    do: Path.expand(path, :code.priv_dir(app_name))

  defp cert({_app_name, _path}), do: nil
  defp cert(nil), do: nil
  defp cert(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      _ -> nil
    end
  end

  defp key({_app_name, _path}), do: nil
  defp key(nil), do: nil
  defp key(bin) do
    case :public_key.pem_decode(bin) do
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> nil
    end
  end

  def apns_keys? do
    mode = Application.get_env(:pigeon, :apns_mode)
    cert = Application.get_env(:pigeon, :apns_cert)
    key = Application.get_env(:pigeon, :apns_key)
    !is_nil(mode) and !is_nil(cert) and !is_nil(key)
  end

  def valid_apns_config?(config) do
    valid_mode? = (config[:mode] == :dev or config[:mode] == :prod)
    valid_cert? = !is_nil(config[:cert] || config[:certfile])
    valid_key? = !is_nil(config[:key] || config[:keyfile])
    valid_mode? and valid_cert? and valid_key?
  end

  def adm_config do
    %{
      client_id: config_adm_client_id,
      client_secret: config_adm_client_secret
    }
  end

  def adm_configured? do
    client_id = Application.get_env(:pigeon, :adm_client_id)
    client_secret = Application.get_env(:pigeon, :adm_client_secret)
    !is_nil(client_id) and !is_nil(client_secret)
  end

  def valid_adm_config?(config) do
    valid_client_id? = is_binary(config[:client_id]) and String.length(config[:client_id]) > 0
    valid_client_secret? = is_binary(config[:client_secret]) and String.length(config[:client_secret]) > 0
    valid_client_id? and valid_client_secret?
  end

  def push(service, notification) do
    case service do
      :apns ->
        GenServer.cast(:apns_worker, {:push, :apns, notification})
      _ ->
        Logger.error "Unknown service #{service}"
    end
  end

  def push(service, notification, on_response) do
    case service do
      :apns ->
        GenServer.cast(:apns_worker, {:push, :apns, notification, on_response})
      _ ->
        Logger.error "Unknown service #{service}"
    end
  end

  def handle_cast(:stop , state), do: { :noreply, state }
end
