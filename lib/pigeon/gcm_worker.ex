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
    Logger.debug("Starting #{name}, key: #{gcm_key}")
    GenServer.start_link(__MODULE__, %{gcm_key: gcm_key}, name: name)
  end

  def stop() do
    :gen_server.cast(self, :stop)
  end

  def init(args) do
    {:ok, args}
  end

  def handle_cast({:push, :gcm, notification}, %{gcm_key: gcm_key} = state) do 
    {:ok, %HTTPoison.Response{body: body}} = HTTPoison.post(gcm_uri, notification, gcm_headers(gcm_key))

    case parse_response(body) do
      {:error, reason} ->
        Logger.error("#{reason}\n#{notification}")
      _ ->
        :ok
    end
    { :noreply, state }
  end

  def handle_cast({:push, :gcm, notification, on_response}, %{gcm_key: gcm_key} = state) do 
    {:ok, %HTTPoison.Response{body: body}} = HTTPoison.post(gcm_uri, notification, gcm_headers(gcm_key))

    case parse_response(body) do
      :ok ->
        on_response.({:ok, notification})
      {:error, reason} ->
        Logger.error("#{reason}\n\n#{notification}")
        on_response.({:error, reason, notification})
    end
    { :noreply, state }
  end

  def parse_response(body) do
    {:ok, json} = Poison.decode(body)
    if json["success"] == 1 do
      :ok
    else
      [result|[]] = json["results"]
      error = result["error"]
      {:error, String.to_atom(error)}
    end
  end

  def handle_cast(:stop, state) do
    { :noreply, state }
  end
end
