defmodule Pigeon.GCM do
  require Logger

  defp gcm_uri, do: 'https://gcm-http.googleapis.com/gcm/send'

  defp gcm_headers(key) do
    [{ "Authorization", "key=#{key}" },
     { "Content-Type", "application/json" },
     { "Accept", "application/json" }]
  end

  @doc """
    Sends a push over GCM
  """
  @spec push(Pigeon.GCM.Notification) :: none
  def push(notification) do
    gcm_key = Application.get_env(:pigeon, :gcm_key)
    requests = encode_requests(notification.registration_id, notification.data)
    response = fn(request) -> 
      HTTPoison.post(gcm_uri, request, gcm_headers(gcm_key))
    end
    for r <- requests, do: response.(r)
    :ok
  end

  @doc """
    Sends a push over GCM and executes function on success/failure.
  """
  @spec push(Pigeon.GCM.Notification, (() -> none)) :: none
  def push(notification, on_response) do
    gcm_key = Application.get_env(:pigeon, :gcm_key)
    requests = encode_requests(notification.registration_id, notification.data)
    response = fn(request) -> 
      {:ok, %HTTPoison.Response{status_code: status, body: body}} = HTTPoison.post(gcm_uri, request, gcm_headers(gcm_key))
      process_response(status, body, notification, on_response)
    end
    for r <- requests, do: response.(r)
  end

  def encode_requests(registration_ids, data) when is_list(registration_ids) do
    chunks = Enum.chunk(registration_ids, 1000, 1000, [])
    Enum.map(chunks, fn(x) -> Poison.encode!(%{registration_ids: x, data: data}) end)
  end
  def encode_requests(registration_id, data) do
    [Poison.encode!(%{to: registration_id, data: data})]
  end

  def process_response(status, body, notification, on_response) do
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
  end

  def handle_error_status_code(reason, notification, on_response) do
    on_response.({:error, reason, notification})
  end

  def handle_200_status(body, %{registration_id: reg_ids} = notification, on_response) when is_list(reg_ids) do
    {:ok, json} = Poison.decode(body)
    results = Enum.zip(notification.registration_id, json["results"])
    for result <- results, do: process_callback(result, notification, on_response)
  end
  def handle_200_status(body, %{registration_id: reg_id} = notification, on_response) do
    {:ok, json} = Poison.decode(body)
    results = Enum.zip([notification.registration_id], json["results"])
    for result <- results, do: process_callback(result, notification, on_response)
  end

  def process_callback({reg_id, response} = result, notification, on_response) do
    case parse_result(response) do
      {:ok, message_id} ->
        notification = %{ notification | registration_id: reg_id, message_id: message_id }
        on_response.({:ok, notification})
      {:ok, message_id, registration_id} ->
        notification = %{ notification | registration_id: reg_id, message_id: message_id, updated_registration_id: registration_id }
        on_response.({:ok, notification})
      {:error, reason} ->
        notification = %{ notification | registration_id: reg_id }
        on_response.({:error, reason, notification})
    end
  end

  def parse_result(result) do
    error = result["error"]
    if is_nil(error) do
      parse_success(result)
    else
      {:error, String.to_atom(error)}
    end
  end

  def parse_success(result) do
    message_id = result["message_id"]
    registration_id = result["registration_id"]
    if is_nil(registration_id) do
      {:ok, message_id}
    else
      {:ok, message_id, registration_id}
    end
  end
end
