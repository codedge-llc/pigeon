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
    HTTPoison.post(gcm_uri, notification, gcm_headers(gcm_key))
    { :noreply, state }
  end

  def handle_cast({:push, :gcm, notification, on_response}, %{gcm_key: gcm_key} = state) do 
    {:ok, %HTTPoison.Response{status_code: status, body: body}} = HTTPoison.post(gcm_uri, notification, gcm_headers(gcm_key))

    case status do
      200 -> 
        handle_200_status(body, notification, on_response)
      400 ->
        handle_error_status_code(:InvalidJSON, notification, on_response)
      401 ->
        handle_error_status_code(:AuthenticationError, notification, on_response)
      500 ->
        handle_error_status_code(:InternalServerError, notification, on_response)
      _ ->
        handle_error_status_code(:UnknownError, notification, on_response)
    end

    { :noreply, state }
  end

  def handle_200_status(body, notification, on_response) do
    case parse_response(body) do
      :ok ->
        on_response.({:ok, notification})
      {:error, reason} ->
        Logger.error("#{reason}\n\n#{notification}")
        on_response.({:error, reason, notification})
    end
  end

  def handle_error_status_code(reason, notification, on_response) do
    Logger.error("#{reason}\n\n#{notification}")
    on_response.({:error, reason, notification})
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
