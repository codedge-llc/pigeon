defmodule Pigeon.ADM do
  @moduledoc """
  `Pigeon.Adapter` for ADM (Amazon Android) push notifications.

  ## Getting Started

  ### Create a dispatcher.

  ```
  # lib/your_app/adm.ex

  defmodule YourApp.ADM do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  ### Configure your dispatcher.

  Configure your `ADM` dispatcher and start it on application boot.

  ```
  # config.exs

  config :your_app, YourApp.ADM,
    adapter: Pigeon.ADM,
    client_id: "your_oauth2_client_id_here",
    client_secret: "your_oauth2_client_secret_here"
  ```

  Add it to your supervision tree.

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

  If preferred, you can include your configuration directly.

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

  ### Create a notification.

  ```
  msg = %{ "body" => "your message" }
  n = Pigeon.ADM.Notification.new("your device registration ID", msg)
  ```

  ### Send the notification.

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

  If the dispatcher has no live connection to ADM, or the access token refresh fails,
  `:response` is `:not_connected` and the push was not sent, so it is safe to resend.
  If the connection is lost after the push was sent, `:response` is `:timeout` and
  delivery is unknown.

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
  alias Pigeon.ADM.{Config, ResultParser, Token}
  alias Pigeon.HTTP.Connection
  require Logger

  @impl true
  def init(opts) do
    config = %Config{
      client_id: Keyword.get(opts, :client_id),
      client_secret: Keyword.get(opts, :client_secret)
    }

    Config.validate!(config)

    conn =
      Connection.new(__MODULE__, fn ->
        Mint.HTTP.connect(:https, "api.amazon.com", 443)
      end)

    {:ok,
     %{
       config: config,
       access_token: nil,
       access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
       access_token_expiration_seconds: 0,
       access_token_type: nil,
       conn: conn
     }}
  end

  @impl true
  def handle_push(notification, state) do
    case refresh_access_token_if_needed(state) do
      {:ok, state} -> {:noreply, do_push(notification, state)}
      {:error, state, reason} -> fail_push(notification, state, reason)
    end
  end

  defp fail_push(notification, state, reason) do
    notification |> Map.put(:response, reason) |> process_on_response()
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, %{conn: conn} = state) do
    case Connection.handle_message(conn, msg, &process_response/1) do
      {:ok, conn} -> {:noreply, %{state | conn: conn}}
      :unknown -> {:noreply, state}
    end
  end

  defp process_response(%{status: 200} = request) do
    %{body: body, notification: notification} = request
    {:ok, json} = Pigeon.json_library().decode(body)

    notification
    |> ResultParser.parse(json)
    |> process_on_response()
  end

  defp process_response(request) do
    %{status: status, body: body, notification: notification} = request

    case Pigeon.json_library().decode(body) do
      {:ok, %{"reason" => _reason} = result_json} ->
        notification
        |> ResultParser.parse(result_json)
        |> process_on_response()

      {:error, _error} ->
        notification
        |> Map.put(:response, generic_error_reason(status))
        |> process_on_response()
    end
  end

  defp generic_error_reason(400), do: :invalid_json
  defp generic_error_reason(401), do: :authentication_error
  defp generic_error_reason(500), do: :internal_server_error
  defp generic_error_reason(_), do: :unknown_error

  defp refresh_access_token_if_needed(state) do
    %{
      access_token: access_token,
      access_token_refreshed_datetime_erl: access_ref_dt_erl,
      access_token_expiration_seconds: access_ref_exp_secs
    } = state

    cond do
      is_nil(access_token) ->
        refresh_access_token(state)

      Token.expired?(access_ref_dt_erl, access_ref_exp_secs) ->
        refresh_access_token(state)

      true ->
        {:ok, state}
    end
  end

  defp refresh_access_token(%{config: config, conn: conn} = state) do
    headers = Token.refresh_headers()
    body = Token.refresh_body(config.client_id, config.client_secret)
    path = "/auth/O2/token"

    case Connection.request_sync(conn, "POST", path, headers, body) do
      {:ok, conn, %{status: 200, body: response_body}} ->
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
             access_token_type: token_type,
             conn: conn
         }}

      {:ok, conn, %{body: response_body}} ->
        {:ok, response_json} = Pigeon.json_library().decode(response_body)
        Logger.error("Refresh token response: #{inspect(response_json)}")
        {:error, %{state | conn: conn}, response_json["reason"]}

      {:error, conn, _reason} ->
        {:error, %{state | conn: conn}, :not_connected}
    end
  end

  defp do_push(notification, %{conn: conn} = state) do
    headers = adm_headers(state)
    body = encode_payload(notification)
    path = adm_path(notification.registration_id)

    %{
      state
      | conn:
          Connection.request(conn, "POST", path, headers, body, notification)
    }
  end

  @spec adm_path(String.t()) :: String.t()
  defp adm_path(reg_id) do
    "/messaging/registrations/#{reg_id}/messages"
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
end
