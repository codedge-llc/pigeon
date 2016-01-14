defmodule Pigeon.GCMWorker do  
  use GenServer
  require Logger

  defp gcm_uri, do: 'https://gcm-http.googleapis.com/gcm/send'

  defp gcm_headers(key) do
    [{ "Authorization", "key=#{key}" },
     { "Content-Type", "application/json" },
     { "Accept", "application/json" }]
  end

  def start_link(name, gcm_key) do
    Logger.debug("Starting worker #{name} with key #{gcm_key}...")
    GenServer.start_link(__MODULE__, %{gcm_key: gcm_key}, name: name)
  end

  def stop() do
    :gen_server.cast(self, :stop)
  end

  def init(args) do
    {:ok, args}
  end

  def handle_call({:push, :gcm, notification}, from, state) do 
    HTTPoison.post!(gcm_uri, notification, gcm_headers(state[:gcm_key]))
    { :reply, :ok, state }
  end

  def handle_cast(:stop , state) do
    { :noreply, state }
  end
end
