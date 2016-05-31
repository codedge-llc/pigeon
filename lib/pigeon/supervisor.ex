defmodule Pigeon.Supervisor do
  @moduledoc """
    Supervises an APNSWorker, restarting as necessary.
  """
  use Supervisor
  require Logger

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: :pigeon)
  end

  def stop do
    :gen_server.cast(:pigeon, :stop)
  end

  def init(:ok) do
    children = []

    if valid_apns_config? do
      apns_mode = Application.get_env(:pigeon, :apns_mode)
      apns_cert = cert_path(:apns_cert)
      apns_key = cert_path(:apns_key)
      apns = worker(Pigeon.APNSWorker,
        [:apns_worker, apns_mode, apns_cert, apns_key],
        id: :apns_worker)

      children = [apns]
    end

    supervise(children, strategy: :one_for_one)
  end

  defp cert_path(config_key) do
    :pigeon
    |> Application.get_env(config_key)
    |> file_path
  end
  defp file_path(path) when is_binary(path), do: Path.expand(path)
  defp file_path({app_name, path}) when is_atom(app_name), do: Path.expand(path, :code.priv_dir(app_name))

  def valid_apns_config? do
    apns_mode = Application.get_env(:pigeon, :apns_mode)
    apns_cert = Application.get_env(:pigeon, :apns_cert)
    apns_key = Application.get_env(:pigeon, :apns_key)
    !is_nil(apns_mode) && !is_nil(apns_cert) && !is_nil(apns_key)
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

  def handle_cast(:stop , state) do
    { :noreply, state }
  end
end
