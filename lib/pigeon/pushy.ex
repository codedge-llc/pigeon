defmodule Pigeon.Pushy do
  @moduledoc """
  `Pigeon.Adapter` for Pushy push notifications.

  This adapter provides support for sending push notifications via the Pushy API.
  It is designed to work with the `Pigeon` library and implements the `Pigeon.Adapter` behaviour.

  ## Example


  Then, you can send a Pushy push notification like this:

      notif = Pigeon.Pushy.Notification.new(%{"message" => "Hello, world!"}, "device_token")

      Pigeon.push(notif)

  ## Configuration

  The following options can be set in the adapter configuration:

  * `:key` - (required) the API key for your Pushy account.
  * `:base_uri` - (optional) the base URI for the Pushy API. Defaults to "api.pushy.me".

  ## Getting Started

  1. Create a Pushy dispatcher.

  ```
  # lib/pushy.ex
  defmodule YourApp.Pushy do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  2. (Optional) Add configuration to your `config.exs`.

  To use this adapter, simply include it in your Pigeon configuration:

      config :your_app, YourApp.Pushy,
        adapter: Pigeon.Pushy,
        key: "pushy secret key"
en

  3. Start your dispatcher on application boot.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.Pushy
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end
  ```

  4. Create a notification.

  ```
  msg = %{ "body" => "your message" }
  n = Pigeon.pushy.Notification.new(msg, "your device token")
  ```

  5. Send the notification.

  ```
  YourApp.Pushy.push(n)
  ```

  ## Handling Push Responses

  1. Pass an optional anonymous function as your second parameter.

  ```
  data = %{ "message" => "your message" }
  n = Pigeon.Pushy.Notification.new(data, "device token")
  YourApp.Pushy.push(n, on_response: fn(x) -> IO.inspect(x) end)
  ```

  2. Responses return a notification with an updated `:response` key.
     You could handle responses like so:

  ```
  on_response_handler = fn(x) ->
    case x.response do
      :success ->
        # Push successful
        :ok
      :failure ->
        # Retry or some other handling for x.failed (devices failed to send)
      :timeout ->
        # request didn't finish within expected time, server didn't respond
      error ->
        # Handle other errors
    end
  end

  data = %{ "message" => "your message" }
  n = Pigeon.Pushy.Notification.new(data, "your device token")
  Pigeon.Pushy.push(n, on_response: on_response_handler)
  ```
  """
  import Pigeon.Tasks, only: [process_on_response: 1]
  require Logger

  alias Pigeon.Pushy.{ResultParser}

  defstruct config: nil

  @behaviour Pigeon.Adapter

  @impl true
  def init(opts) do
    config = Pigeon.Pushy.Config.new(opts)

    Config.validate!(config)

    state = %__MODULE__{config: config}

    {:ok, state}
  end

  @impl true
  def handle_push(notification, state) do
    :ok = do_push(notification, state)
    {:noreply, state}
  end

  @impl true
  def handle_info({_from, {:ok, %HTTPoison.Response{status_code: 200}}}, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp do_push(notification, state) do
    response = fn notification ->
      encoded_notification = encode_requests(notification)

      case HTTPoison.post(
             pushy_uri(state.config),
             encoded_notification,
             pushy_headers()
           ) do
        {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
          process_response(status, body, notification)

        {:error, %HTTPoison.Error{reason: :connect_timeout}} ->
          notification
          |> Map.put(:response, :timeout)
          |> process_on_response()
      end
    end

    Task.Supervisor.start_child(Pigeon.Tasks, fn -> response.(notification) end)
    :ok
  end

  defp pushy_uri(%Pigeon.Pushy.Config{uri: base_uri, key: secret_key}) do
    "https://#{base_uri}/push/?api_key=#{secret_key}"
  end

  def pushy_headers() do
    [
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ]
  end

  defp encode_requests(notif) do
    %{}
    |> encode_to(notif.to)
    |> encode_data(notif.data)
    |> maybe_encode_attr("time_to_live", notif.time_to_live)
    |> maybe_encode_attr("content_available", notif.content_available)
    |> maybe_encode_attr("mutable_content", notif.mutable_content)
    |> maybe_encode_attr("notification", notif.notification)
    |> maybe_encode_attr("schedule", notif.schedule)
    |> maybe_encode_attr("collapse_key", notif.collapse_key)
    |> Pigeon.json_library().encode!()
  end

  defp encode_to(map, value) do
    Map.put(map, "to", value)
  end

  defp encode_data(map, value) do
    Map.put(map, "data", value)
  end

  defp maybe_encode_attr(map, _key, nil), do: map

  defp maybe_encode_attr(map, key, val) do
    Map.put(map, key, val)
  end

  defp process_response(200, body, notification),
    do: handle_200_status(body, notification)

  defp process_response(status, body, notification),
    do: handle_error_status_code(status, body, notification)

  defp handle_200_status(body, notification) do
    {:ok, json} = Pigeon.json_library().decode(body)

    ResultParser.parse(notification, json)
    |> process_on_response()
  end

  defp handle_error_status_code(status, body, notification) do
    case Pigeon.json_library().decode(body) do
      {:ok, %{"error" => _reason} = result_json} ->
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
