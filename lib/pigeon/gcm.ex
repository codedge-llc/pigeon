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
    requests =
      chunk_registration_ids(notification.registration_id)
      |> encode_requests(notification.data)

    response = fn({reg_ids, payload} = request) -> HTTPoison.post(gcm_uri, payload, gcm_headers(gcm_key)) end
    for r <- requests, do: Task.async(fn -> response.(r) end)
    :ok
  end

  @doc """
    Sends a push over GCM and executes function on success/failure.
  """
  @spec push(Pigeon.GCM.Notification, (() -> none)) :: none
  def push(notification, on_response) do
    gcm_key = Application.get_env(:pigeon, :gcm_key)
    requests =
      chunk_registration_ids(notification.registration_id)
      |> encode_requests(notification.data)

    response = fn({reg_ids, payload} = request) -> 
      {:ok, %HTTPoison.Response{status_code: status, body: body}} = HTTPoison.post(gcm_uri, payload, gcm_headers(gcm_key))
      notification = %{ notification | registration_id: reg_ids }
      process_response(status, body, notification, on_response)
    end
    for r <- requests, do: Task.async(fn -> response.(r) end)
    :ok
  end

  def chunk_registration_ids(reg_ids) when is_binary(reg_ids), do: [[reg_ids]]
  def chunk_registration_ids(reg_ids), do: Enum.chunk(reg_ids, 1000, 1000, [])

  def encode_requests([[reg_id]|_rest] = registration_ids, data) do
    [{reg_id, Poison.encode!(%{to: reg_id, data: data})}]
  end
  def encode_requests(registration_ids, data) do
    Enum.map(registration_ids, fn(x) -> {x, Poison.encode!(%{registration_ids: x, data: data})} end)
  end

  def process_response(status, body, notification, on_response) do
    case status do
      200 -> 
        handle_200_status(body, notification, on_response)
      400 ->
        handle_error_status_code(:invalid_jSON, notification, on_response)
      401 ->
        handle_error_status_code(:authentication_error, notification, on_response)
      500 ->
        handle_error_status_code(:internal_server_error, notification, on_response)
      _ ->
        handle_error_status_code(:unknown_error, notification, on_response)
    end
  end

  def handle_error_status_code(reason, notification, on_response), do: on_response.({:error, reason, notification})

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
      error_atom = error |> Mix.Utils.underscore |> String.to_atom
      {:error, error_atom}
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
