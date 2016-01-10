defmodule Pigeon.Server do  
  use Supervisor
  require Logger

  @doc "Starts the server"
  def start_link() do
    # Delegate to gen_server passing in the current module. ARG will
    # be passed to init
    Logger.debug("Starting link...")
    Supervisor.start_link(__MODULE__, :ok, name: :pigeon)
  end

  @doc "Stops the server"
  def stop() do
    :gen_server.cast(:pigeon, :stop)
  end

  @doc "Initialize our server"
  def init(:ok) do
    gcm_key = Application.get_env(:pigeon, :gcm_key)
    gcm = worker(Pigeon.GCMWorker, [:gcm_worker, gcm_key], id: :gcm_worker)

    apns_mode = Application.get_env(:pigeon, :apns_mode)
    apns_cert = Application.get_env(:pigeon, :apns_cert)
    apns_key = Application.get_env(:pigeon, :apns_key)
    apns = worker(Pigeon.APNSWorker, [:apns_worker, apns_mode, apns_cert, apns_key], id: :apns_worker)

    apns_feedback = worker(Pigeon.APNSFeedback, [:apns_feedback, apns_mode, apns_cert, apns_key], id: :apns_feedback)
    
    supervise([apns, gcm, apns_feedback], strategy: :one_for_one)
  end

  @doc "Implement this multiple times with a different pattern to deal
  with sync messages"
  def push(service, notification) do 
    case service do
      :apns ->
        GenServer.cast(:apns_feedback, :listen)
        GenServer.call(:apns_worker, {:push, :apns, notification})
      _ ->
        Logger.debug "Unknown service #{service}"
    end
  end

  def handle_call(message, from, state) do
    {:reply, {:error, :bad_message}, state}
  end

  @doc "Implement this multiple times with a different pattern to deal
  with async messages"
  def handle_cast(:message, state) do
  end

  @doc "Handle the server stop message"
  def handle_cast(:stop , state) do
    { :noreply, state }
  end

  @doc "Implement this to handle out of band messages (messages not
  sent using a gen_server call)"
  def handle_info(message, state) do
    {:reply, {:error, :bad_message}, state}
  end
end  
