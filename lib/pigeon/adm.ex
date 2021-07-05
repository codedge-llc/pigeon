defmodule Pigeon.ADM do
  @moduledoc """
  `Pigeon.Adapter` for ADM (Amazon Android) push notifications.

  ## Getting Started

  1. Create an ADM dispatcher.

  ```
  # lib/adm.ex
  defmodule YourApp.ADM do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  2. (Optional) Add configuration to your `config.exs`.

  ```
  # config.exs

  config :your_app, YourApp.ADM,
    adapter: Pigeon.ADM,
    client_id: "your_oauth2_client_id_here",
    client_secret: "your_oauth2_client_secret_here"
  ```

  3. Start your dispatcher on application boot.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.ADM
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end
  ```

  If you skipped step two, include your configuration.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        {YourApp.ADM, adm_opts()}
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp adm_opts do
      [
        adapter: Pigeon.ADM, 
        client_id: "client_id", 
        client_secret: "secret"
      ]
    end
  end
  ```

  4. Create a notification.

  ```
  msg = %{ "body" => "your message" }
  n = Pigeon.ADM.Notification.new("your device registration ID", msg)
  ```

  5. Send the notification.

  ```
  YourApp.ADM.push(n)
  ```

  ## Handling Push Responses

  1. Pass an optional anonymous function as your second parameter.

  ```
  data = %{ message: "your message" }
  n = Pigeon.ADM.Notification.new("device registration ID", data)
  YourApp.ADM.push(n, on_response: fn(x) -> IO.inspect(x) end)
  ```

  2. Responses return a notification with an updated `:response` key.
     You could handle responses like so:

  ```
  on_response_handler = fn(x) ->
    case x.response do
      :success ->
        # Push successful
        :ok
      :update ->
        new_reg_id = x.updated_registration_id
        # Update the registration ID in the database
      :invalid_registration_id ->
        # Remove the bad ID from the database
      :unregistered ->
        # Remove the bad ID from the database
      error ->
        # Handle other errors
    end
  end

  data = %{ message: "your message" }
  n = Pigeon.ADM.Notification.new("your registration id", data)
  Pigeon.ADM.push(n, on_response: on_response_handler)
  ```

  ## Error Responses

  *Taken from [Amazon Device Messaging docs](https://developer.amazon.com/public/apis/engage/device-messaging/tech-docs/06-sending-a-message)*

  | Reason                           | Description                      |
  |----------------------------------|----------------------------------|
  | `:invalid_registration_id`       | Invalid Registration Token       |
  | `:invalid_data`                  | Bad format JSON data             |
  | `:invalid_consolidation_key`     | Invalid Consolidation Key        |
  | `:invalid_expiration`            | Invalid expiresAfter value       |
  | `:invalid_checksum`              | Invalid md5 value                |
  | `:invalid_type`                  | Invalid Type header              |
  | `:unregistered`                  | App instance no longer available |
  | `:access_token_expired`          | Expired OAuth access token       |
  | `:message_too_large`             | Data size exceeds 6 KB           |
  | `:max_rate_exceeded`             | See Retry-After response header  |
  | `:unknown_error`                 | Unknown Error                    |
  """

  @behaviour Pigeon.Adapter

  import Pigeon.Tasks, only: [process_on_response: 1]
  alias Pigeon.ADM.{Config, ResultParser}
  require Logger

  @token_refresh_uri "https://api.amazon.com/auth/O2/token"
  @token_refresh_early_seconds 5

  @impl true
  def init(opts) do
    config = %Config{
      client_id: Keyword.get(opts, :client_id),
      client_secret: Keyword.get(opts, :client_secret)
    }

    Config.validate!(config)

    {:ok,
     %{
       config: config,
       access_token: nil,
       access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
       access_token_expiration_seconds: 0,
       access_token_type: nil
     }}
  end

  @impl true
  def handle_push(notification, state) do
    case refresh_access_token_if_needed(state) do
      {:ok, state} ->
        :ok = do_push(notification, state)
        {:noreply, state}

      {:error, reason} ->
        notification
        |> Map.put(:response, reason)
        |> process_on_response()

        {:noreply, state}
    end
  end

  @impl true
  def handle_info({_from, {:ok, %HTTPoison.Response{status_code: 200}}}, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp refresh_access_token_if_needed(state) do
    %{
      access_token: access_token,
      access_token_refreshed_datetime_erl: access_ref_dt_erl,
      access_token_expiration_seconds: access_ref_exp_secs
    } = state

    cond do
      is_nil(access_token) ->
        refresh_access_token(state)

      access_token_expired?(access_ref_dt_erl, access_ref_exp_secs) ->
        refresh_access_token(state)

      true ->
        {:ok, state}
    end
  end

  defp access_token_expired?(_refreshed_datetime_erl, 0), do: true

  defp access_token_expired?(refreshed_datetime_erl, expiration_seconds) do
    seconds_since(refreshed_datetime_erl) >=
      expiration_seconds - @token_refresh_early_seconds
  end

  defp seconds_since(datetime_erl) do
    gregorian_seconds =
      datetime_erl
      |> :calendar.datetime_to_gregorian_seconds()

    now_gregorian_seconds =
      :os.timestamp()
      |> :calendar.now_to_universal_time()
      |> :calendar.datetime_to_gregorian_seconds()

    now_gregorian_seconds - gregorian_seconds
  end

  defp refresh_access_token(state) do
    post =
      HTTPoison.post(
        @token_refresh_uri,
        token_refresh_body(state),
        token_refresh_headers()
      )

    case post do
      {:ok, %{status_code: 200, body: response_body}} ->
        {:ok, response_json} = Pigeon.json_library().decode(response_body)

        %{
          "access_token" => access_token,
          "expires_in" => expiration_seconds,
          "scope" => _scope,
          "token_type" => token_type
        } = response_json

        now_datetime_erl = :os.timestamp() |> :calendar.now_to_universal_time()

        {:ok,
         %{
           state
           | access_token: access_token,
             access_token_refreshed_datetime_erl: now_datetime_erl,
             access_token_expiration_seconds: expiration_seconds,
             access_token_type: token_type
         }}

      {:ok, %{body: response_body}} ->
        {:ok, response_json} = Pigeon.json_library().decode(response_body)
        Logger.error("Refresh token response: #{inspect(response_json)}")
        {:error, response_json["reason"]}
    end
  end

  defp token_refresh_body(%{
         config: %{client_id: client_id, client_secret: client_secret}
       }) do
    %{
      "grant_type" => "client_credentials",
      "scope" => "messaging:push",
      "client_id" => client_id,
      "client_secret" => client_secret
    }
    |> URI.encode_query()
  end

  defp token_refresh_headers do
    [{"Content-Type", "application/x-www-form-urlencoded;charset=UTF-8"}]
  end

  defp do_push(notification, state) do
    request = {notification.registration_id, encode_payload(notification)}

    response = fn {reg_id, payload} ->
      case HTTPoison.post(adm_uri(reg_id), payload, adm_headers(state)) do
        {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
          notification = %{notification | registration_id: reg_id}
          process_response(status, body, notification)

        {:error, %HTTPoison.Error{reason: :connect_timeout}} ->
          notification
          |> Map.put(:response, :timeout)
          |> process_on_response()
      end
    end

    Task.Supervisor.start_child(Pigeon.Tasks, fn -> response.(request) end)
    :ok
  end

  defp adm_uri(reg_id) do
    "https://api.amazon.com/messaging/registrations/#{reg_id}/messages"
  end

  defp adm_headers(%{access_token: access_token, access_token_type: token_type}) do
    [
      {"Authorization", "#{token_type} #{access_token}"},
      {"Content-Type", "application/json"},
      {"X-Amzn-Type-Version", "com.amazon.device.messaging.ADMMessage@1.0"},
      {"Accept", "application/json"},
      {"X-Amzn-Accept-Type", "com.amazon.device.messaging.ADMSendResult@1.0"}
    ]
  end

  defp encode_payload(notification) do
    notification.payload
    |> put_consolidation_key(notification.consolidation_key)
    |> put_expires_after(notification.expires_after)
    |> put_md5(notification.md5)
    |> Pigeon.json_library().encode!()
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

  defp process_response(200, body, notification),
    do: handle_200_status(body, notification)

  defp process_response(status, body, notification),
    do: handle_error_status_code(status, body, notification)

  defp handle_200_status(body, notification) do
    {:ok, json} = Pigeon.json_library().decode(body)

    notification
    |> ResultParser.parse(json)
    |> process_on_response()
  end

  defp handle_error_status_code(status, body, notification) do
    case Pigeon.json_library().decode(body) do
      {:ok, %{"reason" => _reason} = result_json} ->
        notification
        |> ResultParser.parse(result_json)
        |> process_on_response()

      {:error, _} ->
        notification
        |> Map.put(:response, generic_error_reason(status))
        |> process_on_response()
    end
  end

  defp generic_error_reason(400), do: :invalid_json
  defp generic_error_reason(401), do: :authentication_error
  defp generic_error_reason(500), do: :internal_server_error
  defp generic_error_reason(_), do: :unknown_error
end
