defmodule Pigeon.Supervisor do  
  use Supervisor
  require Logger

  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: :pigeon)
  end

  def stop() do
    :gen_server.cast(:pigeon, :stop)
  end

  def init(:ok) do
    children = []

    if valid_gcm_config? do
      gcm_key = Application.get_env(:pigeon, :gcm_key)
      gcm = worker(Pigeon.GCMWorker, [:gcm_worker, gcm_key], id: :gcm_worker)
      children = [gcm | children]
    end
    
    if valid_apns_config? do
      apns_mode = Application.get_env(:pigeon, :apns_mode)
      apns_cert = Application.get_env(:pigeon, :apns_cert)
      apns_key = Application.get_env(:pigeon, :apns_key)
      apns = worker(Pigeon.APNSWorker, [:apns_worker, apns_mode, apns_cert, apns_key], id: :apns_worker)
      children = [apns | children]
    end

    supervise(children, strategy: :one_for_one)
  end

  def valid_gcm_config? do
    gcm_key = Application.get_env(:pigeon, :gcm_key) |> is_nil
    !gcm_key
  end
  
  def valid_apns_config? do
    apns_mode = Application.get_env(:pigeon, :apns_mode) |> is_nil
    apns_cert = Application.get_env(:pigeon, :apns_cert) |> is_nil
    apns_key = Application.get_env(:pigeon, :apns_key) |> is_nil
    !apns_mode && !apns_cert && !apns_key
  end

  def push(service, notification) do 
    case service do
      :apns ->
        GenServer.call(:apns_worker, {:push, :apns, notification})
      :gcm ->
        GenServer.call(:gcm_worker, {:push, :gcm, notification})
      _ ->
        Logger.error "Unknown service #{service}"
    end
  end

  def handle_cast(:stop , state) do
    { :noreply, state }
  end
end  
