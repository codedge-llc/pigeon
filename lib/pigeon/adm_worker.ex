defmodule Pigeon.ADMWorker do
  @moduledoc """
    Handles all Amazon ADM request and response parsing.
    Includes managing OAuth2 tokens.
  """
  use GenServer
  require Logger

  @token_refresh_uri "https://api.amazon.com/auth/O2/token"

  def start_link(name, config) do
    GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  end

  def stop, do: :gen_server.cast(self, :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    {:ok, %{
      client_id: config[:client_id],
      client_secret: config[:client_secret],
      access_token: nil,
      access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
      access_token_expiration_seconds: 0,
      access_token_type: nil
    }}
  end

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, :adm, notification}, state) do
    case refresh_access_token_if_needed(state) do
      {:ok, state} ->
        :ok = do_push(notification, state, nil)
        {:noreply, state}
      {:error, _reason} ->
        {:noreply, state}
    end
  end

  def handle_cast({:push, :adm, notification, on_response}, state) do
    case refresh_access_token_if_needed(state) do
      {:ok, state} ->
        :ok = do_push(notification, state, on_response)
        {:noreply, state}
      {:error, reason} ->
        on_response.({:error, reason, notification})
        {:noreply, state}
    end
  end

  defp refresh_access_token_if_needed(state) do
    %{
      access_token: access_token,
      access_token_refreshed_datetime_erl: access_token_refreshed_datetime_erl,
      access_token_expiration_seconds: access_token_expiration_seconds
    } = state

    cond do
      is_nil(access_token) ->
        refresh_access_token(state)
      access_token_expired?(access_token_refreshed_datetime_erl, access_token_expiration_seconds) ->
        refresh_access_token(state)
      true ->
        {:ok, state}
    end
  end

  defp access_token_expired?(_refreshed_datetime_erl, 0), do: true
  defp access_token_expired?(refreshed_datetime_erl, expiration_seconds) do
    seconds_since(refreshed_datetime_erl) >= expiration_seconds
  end

  defp seconds_since(datetime_erl) do
    gregorian_seconds =
      datetime_erl
      |> :calendar.datetime_to_gregorian_seconds

    now_gregorian_seconds =
      :os.timestamp
      |> :calendar.now_to_universal_time
      |> :calendar.datetime_to_gregorian_seconds

    now_gregorian_seconds - gregorian_seconds
  end

  defp refresh_access_token(state) do
    case HTTPoison.post(@token_refresh_uri, token_refresh_body(state), token_refresh_headers()) do
      {:ok, %{status_code: 200, body: response_body}} ->
        {:ok, response_json} = Poison.decode(response_body)
        %{
          "access_token" => access_token,
          "expires_in" => expiration_seconds,
          "scope" => _scope,
          "token_type" => token_type
        } = response_json

        now_datetime_erl = :os.timestamp |> :calendar.now_to_universal_time

        {:ok, %{state | access_token: access_token,
                        access_token_refreshed_datetime_erl: now_datetime_erl,
                        access_token_expiration_seconds: expiration_seconds,
                        access_token_type: token_type}}

      {:ok, %{body: response_body}} ->
        {:ok, response_json} = Poison.decode(response_body)
        Logger.error "Refresh token response: #{inspect response_json}"
        {:error, response_json["reason"]}
    end
  end

  defp token_refresh_body(%{client_id: client_id, client_secret: client_secret}) do
    %{
      "grant_type" => "client_credentials",
      "scope" => "messaging:push",
      "client_id" => client_id,
      "client_secret" => client_secret
    }
    |> URI.encode_query
  end

  defp token_refresh_headers do
    [{"Content-Type", "application/x-www-form-urlencoded;charset=UTF-8"}]
  end

  defp do_push(notification, state, on_response) do
    request = {notification.registration_id, encode_payload(notification)}

    response =
      case on_response do
        nil ->
          fn({reg_id, payload}) ->
            HTTPoison.post(adm_uri(reg_id), payload, adm_headers(state))
          end
        _ ->
          fn({reg_id, payload}) ->
            {:ok, %HTTPoison.Response{status_code: status, body: body}} =
              HTTPoison.post(adm_uri(reg_id), payload, adm_headers(state))

            notification = %{ notification | registration_id: reg_id }
            process_response(status, body, notification, on_response)
          end
      end
    Task.async(fn -> response.(request) end)
    :ok
  end

  defp adm_uri(reg_id) do
    "https://api.amazon.com/messaging/registrations/#{reg_id}/messages"
  end

  defp adm_headers(%{access_token: access_token, access_token_type: token_type}) do
    [{"Authorization", "#{token_type} #{access_token}"},
     {"Content-Type", "application/json"},
     {"X-Amzn-Type-Version", "com.amazon.device.messaging.ADMMessage@1.0"},
     {"Accept", "application/json"},
     {"X-Amzn-Accept-Type", "com.amazon.device.messaging.ADMSendResult@1.0"}]
  end

  defp encode_payload(notification) do
    notification.payload
    |> put_consolidation_key(notification.consolidation_key)
    |> put_expires_after(notification.expires_after)
    |> put_md5(notification.md5)
    |> Poison.encode!
  end

  defp put_consolidation_key(payload, nil), do: payload
  defp put_consolidation_key(payload, consolidation_key) do
    payload |> Map.put("consolidationKey", consolidation_key)
  end

  defp put_expires_after(payload, nil), do: payload
  defp put_expires_after(payload, expires_after) do
    payload |> Map.put("expiresAfter", expires_after)
  end

  defp put_md5(payload, nil), do: payload
  defp put_md5(payload, md5) do
    payload |> Map.put("md5", md5)
  end

  defp process_response(200, body, notification, on_response),
    do: handle_200_status(body, notification, on_response)
  defp process_response(status, body, notification, on_response),
    do: handle_error_status_code(status, body, notification, on_response)

  defp handle_error_status_code(status, body, notification, on_response) do
    case Poison.decode(body) do
      {:ok, %{"reason" => reason}} ->
        reason_atom = reason |> Macro.underscore |> String.to_atom
        on_response.({:error, reason_atom, notification})
      {:error, _} ->
        on_response.({:error, generic_error_reason(status), notification})
    end
  end

  defp generic_error_reason(400), do: :invalid_json
  defp generic_error_reason(401), do: :authentication_error
  defp generic_error_reason(500), do: :internal_server_error
  defp generic_error_reason(_), do: :unknown_error

  defp handle_200_status(body, notification, on_response) do
    {:ok, json} = Poison.decode(body)
    process_callback({notification.registration_id, json}, notification, on_response)
  end

  defp process_callback({reg_id, response}, notification, on_response) do
    case parse_result(response) do
      :ok ->
        notification = %{ notification | registration_id: reg_id }
        on_response.({:ok, notification})

      {:ok, registration_id} ->
        notification =
          %{ notification | registration_id: reg_id, updated_registration_id: registration_id }
        on_response.({:ok, notification})

      {:error, reason} ->
        notification = %{ notification | registration_id: reg_id }
        on_response.({:error, reason, notification})
    end
  end

  defp parse_result(result) do
    error = result["reason"]
    if is_nil(error) do
      parse_success(result)
    else
      error_atom = error |> Macro.underscore |> String.to_atom
      {:error, error_atom}
    end
  end

  defp parse_success(result) do
    registration_id = result["registrationID"]
    if is_nil(registration_id) do
      :ok
    else
      {:ok, registration_id}
    end
  end
end
