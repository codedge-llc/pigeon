defmodule Pigeon.LegacyFCM do
  @moduledoc """
  `Pigeon.Adapter` for Legacy Firebase Cloud Messaging (FCM) push notifications.

  ## Getting Started

  1. Create a `LegacyFCM` dispatcher.

  ```
  # lib/legacy_fcm.ex
  defmodule YourApp.LegacyFCM do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  2. (Optional) Add configuration to your `config.exs`.

  ```
  # config.exs

  config :your_app, YourApp.LegacyFCM,
    adapter: Pigeon.LegacyFCM,
    key: "your_fcm_key_here"
  ```

  3. Start your dispatcher on application boot.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.LegacyFCM
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
        {YourApp.ADM, legacy_fcm_opts()}
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp legacy_fcm_opts do
      [
        adapter: Pigeon.LegacyFCM, 
        key: "your_fcm_key_here"
      ]
    end
  end
  ```

  4. Create a notification.

  ```
  msg = %{"body" => "your message"}
  n = Pigeon.LegacyFCM.Notification.new("your device registration ID", msg)
  ```
   
  5. Send the notification. 

  Pushes are synchronous and return the notification with
  updated `:status` and `:response` keys. If `:status` is success, `:response`
  will contain a keyword list of individual registration ID responses.

  ```
  YourApp.LegacyFCM.push(n)
  ```

  ## Sending to Multiple Registration IDs

  Pass in a list of registration IDs, as many as you want.

  ```
  msg = %{"body" => "your message"}
  n = Pigeon.FCM.Notification.new(["first ID", "second ID"], msg)
  ```

  ## Notification Struct

  ```
  %Pigeon.LegacyFCM.Notification{
    collapse_key: nil | String.t(),
    dry_run: boolean,
    message_id: nil | String.t(),
    payload: %{...},
    priority: :normal | :high,
    registration_id: String.t() | [String.t(), ...],
    response: [] | [{atom, String.t()}, ...], | atom,
    restricted_package_name: nil | String.t(),
    status: atom | nil,
    time_to_live: non_neg_integer
  }
  ```

  ## Notifications with Custom Data

  FCM accepts both `notification` and `data` keys in its JSON payload. Set them like so:

  ```
  notification = %{"body" => "your message"}
  data = %{"key" => "value"}
  Pigeon.LegacyFCM.Notification.new("registration ID", notification, data)
  ```

  or

  ```
  Pigeon.LegacyFCM.Notification.new("registration ID")
  |> put_notification(%{"body" => "your message"})
  |> put_data(%{"key" => "value"})
  ```

  ## Handling Push Responses

  1. Pass an optional anonymous function as your second parameter.

  ```
  data = %{message: "your message"}
  n = Pigeon.FCM.Notification.new(data, "device registration ID")
  Pigeon.FCM.push(n, fn(x) -> IO.inspect(x) end)
  {:ok, %Pigeon.FCM.Notification{...}}
  ```

  2. Reponses return the notification with an updated response.

  ```
  on_response = fn(n) ->
    case n.status do
      :success ->
        bad_regids = FCM.Notification.remove?(n)
        to_retry = FCM.Notification.retry?(n)
        # Handle updated regids, remove bad ones, etc
      :unauthorized ->
        # Bad FCM key
      error ->
        # Some other error
    end
  end

  data = %{message: "your message"}
  n = Pigeon.FCM.Notification.new("your device token", data)
  Pigeon.FCM.push(n, on_response: on_response)
  ```

  ## Error Responses

  *Slightly modified from [FCM Server Reference](https://firebase.google.com/docs/cloud-messaging/http-server-ref#error-codes)*

  | Reason                           | Description                  |
  |----------------------------------|------------------------------|
  | `:missing_registration`          | Missing Registration Token   |
  | `:invalid_registration`          | Invalid Registration Token   |
  | `:not_registered`                | Unregistered Device          |
  | `:invalid_package_name`          | Invalid Package Name         |
  | `:authentication_error`          | Authentication Error         |
  | `:mismatch_sender_id`            | Mismatched Sender            |
  | `:invalid_json`                  | Invalid JSON                 |
  | `:message_too_big`               | Message Too Big              |
  | `:invalid_data_key`              | Invalid Data Key             |
  | `:invalid_ttl`                   | Invalid Time to Live         |
  | `:unavailable`                   | Timeout                      |
  | `:internal_server_error`         | Internal Server Error        |
  | `:device_message_rate_exceeded`  | Message Rate Exceeded        |
  | `:topics_message_rate_exceeded`  | Topics Message Rate Exceeded |
  | `:unknown_error`                 | Unknown Error                |
  """

  defstruct queue: Pigeon.NotificationQueue.new(),
            stream_id: 1,
            socket: nil,
            config: nil

  @behaviour Pigeon.Adapter

  alias Pigeon.{Configurable, NotificationQueue}
  alias Pigeon.Http2.{Client, Stream}

  @impl true
  def init(opts) do
    config = Pigeon.LegacyFCM.Config.new(opts)
    Configurable.validate!(config)

    state = %__MODULE__{config: config}

    case connect_socket(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:ok, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_push(notification, %{config: config, queue: queue} = state) do
    headers = Configurable.push_headers(config, notification, [])
    payload = Configurable.push_payload(config, notification, [])

    Client.default().send_request(state.socket, headers, payload)

    new_q = NotificationQueue.add(queue, state.stream_id, notification)

    state =
      state
      |> inc_stream_id()
      |> Map.put(:queue, new_q)

    {:noreply, state}
  end

  def handle_info(:ping, state) do
    Client.default().send_ping(state.socket)
    Configurable.schedule_ping(state.config)

    {:noreply, state}
  end

  def handle_info({:closed, _}, %{config: config} = state) do
    case connect_socket(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:noreply, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_info(msg, state) do
    case Client.default().handle_end_stream(msg, state) do
      {:ok, %Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  defp connect_socket(config), do: connect_socket(config, 0)

  defp connect_socket(_config, 3), do: {:error, :timeout}

  defp connect_socket(config, tries) do
    case Configurable.connect(config) do
      {:ok, socket} -> {:ok, socket}
      {:error, _reason} -> connect_socket(config, tries + 1)
    end
  end

  @doc false
  def process_end_stream(%Stream{id: stream_id} = stream, state) do
    %{queue: queue, config: config} = state

    case NotificationQueue.pop(queue, stream_id) do
      {nil, new_queue} ->
        # Do nothing if no queued item for stream
        {:noreply, %{state | queue: new_queue}}

      {notif, new_queue} ->
        Configurable.handle_end_stream(config, stream, notif)
        {:noreply, %{state | queue: new_queue}}
    end
  end

  @doc false
  def inc_stream_id(%{stream_id: stream_id} = state) do
    %{state | stream_id: stream_id + 2}
  end
end
