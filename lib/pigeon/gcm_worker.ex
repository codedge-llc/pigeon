defmodule Pigeon.GCMWorker do  
  use GenServer
  require Logger

  @doc "Starts the worker"
  def start_link(name, gcm_key) do
    Logger.debug("Starting worker #{name} with key #{gcm_key}...")
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @doc "Stops the server"
  def stop() do
    :gen_server.cast(self, :stop)
  end

  @doc "Initialize our server"
  def init(:ok) do
    {:ok, self}
  end

  @doc "Implement this multiple times with a different pattern to deal
  with sync messages"
  def handle_call(:message, from, state) do 
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
