defmodule Pigeon.GCM do
  @moduledoc """
    Handles all Google Cloud Messaging (GCM) request and response functionality.
  """
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
    do_push(notification, %{gcm_key: gcm_key})
  end

  @doc """
    Sends a push over GCM and executes function on success/failure.
  """
  @spec push(Pigeon.GCM.Notification, (() -> none)) :: none
  def push(notification, on_response) when is_function(on_response) do
    gcm_key = Application.get_env(:pigeon, :gcm_key)
    do_push(notification, %{gcm_key: gcm_key}, on_response)
  end

  def push(notification, config, on_response \\ nil) do
    do_push(notification, config, on_response)
  end

  defp do_push(notification, %{gcm_key: gcm_key}, on_response \\ nil) do
    requests =
      notification.registration_id
      |> chunk_registration_ids
      |> encode_requests(notification.payload)

    response =
      case on_response do
        nil ->
          fn({_reg_ids, payload}) ->
            HTTPoison.post(gcm_uri, payload, gcm_headers(gcm_key))
          end
        _ ->
          fn({reg_ids, payload}) ->
            {:ok, %HTTPoison.Response{status_code: status, body: body}} =
              HTTPoison.post(gcm_uri, payload, gcm_headers(gcm_key))

            notification = %{ notification | registration_id: reg_ids }
            process_response(status, body, notification, on_response)
          end
      end
    for r <- requests, do: Task.async(fn -> response.(r) end)
    :ok
  end

  def chunk_registration_ids(reg_ids) when is_binary(reg_ids), do: [[reg_ids]]
  def chunk_registration_ids(reg_ids), do: Enum.chunk(reg_ids, 1000, 1000, [])

  def encode_requests([[reg_id]|_rest], payload) do
    to_send = Map.merge(%{"to" => reg_id}, payload)
    [{reg_id, Poison.encode!(to_send)}]
  end
  def encode_requests(registration_ids, payload) do
    Enum.map(registration_ids, fn(x) -> encode_payload(x, payload) end)
  end

  defp encode_payload(x, payload) do
    encoded =
      %{"registration_ids" => x}
      |> Map.merge(payload)
      |> Poison.encode!
    {x, encoded}
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

  def handle_error_status_code(reason, notification, on_response),
    do: on_response.({:error, reason, notification})

  def handle_200_status(body, %{registration_id: reg_id} = n, on_response) when is_list(reg_id) do
    {:ok, json} = Poison.decode(body)
    results = Enum.zip(n.registration_id, json["results"])
    for result <- results, do: process_callback(result, n, on_response)
  end
  def handle_200_status(body, %{registration_id: _reg_id} = notification, on_response) do
    {:ok, json} = Poison.decode(body)
    results = Enum.zip([notification.registration_id], json["results"])
    for result <- results, do: process_callback(result, notification, on_response)
  end

  def process_callback({reg_id, response}, notification, on_response) do
    case parse_result(response) do
      {:ok, message_id} ->
        notification = %{ notification | registration_id: reg_id, message_id: message_id }
        on_response.({:ok, notification})

      {:ok, message_id, registration_id} ->
        notification =
          %{ notification | registration_id: reg_id,
          message_id: message_id,
          updated_registration_id: registration_id }
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
      error_atom = error |> Macro.underscore |> String.to_existing_atom
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
