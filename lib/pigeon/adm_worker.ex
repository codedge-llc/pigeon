defmodule Pigeon.ADMWorker do
  @moduledoc """
    Handles all Amazon ADM request and response parsing.
    Includes managing OAuth2 tokens.
  """
  use GenServer

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
      {:ok, state} -> do_push(notification, state, nil)
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:push, :adm, notification, on_response}, state) do
    case refresh_access_token_if_needed(state) do
      {:ok, state} ->
        do_push(notification, state, on_response)
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
      true -> state
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
          "scope" => scope,
          "token_type" => token_type
        } = response_json

        now_datetime_erl = :os.timestamp |> :calendar.now_to_universal_time

        {:ok, %{state | access_token: access_token,
                        access_token_refreshed_datetime_erl: now_datetime_erl,
                        access_token_expiration_seconds: expiration_seconds,
                        access_token_type: token_type}}

      {:ok, %{body: response_body}} ->
        {:ok, response_json} = Poison.decode(response_body)
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
    requests =
      notification.registration_id
      |> chunk_registration_ids
      |> encode_requests(notification.payload)

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
    for r <- requests, do: Task.async(fn -> response.(r) end)
    :ok
  end

  defp adm_uri(reg_id) do
    "https://api.amazon.com/messaging/registrations/#{reg_id}/messages"
  end

  defp adm_headers(%{access_token: access_token, access_token_type: token_type}) do
    [{"Authorization", "#{token_type} #{access_token}"}]
  end

  # Amazon ADM does not support batch sending
  def chunk_registration_ids(reg_ids) when is_binary(reg_ids), do: [[reg_ids]]
  def chunk_registration_ids(reg_ids), do: Enum.chunk(reg_ids, 1, 1, [])

  # TODO
  def encode_requests([[reg_id]|_rest], payload) do
    to_send = Map.merge(%{"to" => reg_id}, payload)
    [{reg_id, Poison.encode!(to_send)}]
  end
  def encode_requests(registration_ids, payload) do
    Enum.map(registration_ids, fn(x) -> encode_payload(x, payload) end)
  end

  # TODO
  defp encode_payload(x, payload) do
    encoded =
      %{"registration_ids" => x}
      |> Map.merge(payload)
      |> Poison.encode!
    {x, encoded}
  end

  # TODO
  defp process_response(status, body, notification, on_response) do
    :todo
  end
end