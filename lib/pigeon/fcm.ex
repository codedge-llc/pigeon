defmodule Pigeon.FCM do
  @moduledoc """
  `Pigeon.Adapter` for Firebase Cloud Messaging (FCM) push notifications.

  ## Getting Started

  1. Create a `FCM` dispatcher.

  ```
  # lib/fcm.ex
  defmodule YourApp.FCM do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  2. (Optional) Add configuration to your `config.exs`.

  ```
  # config.exs

  config :your_app, YourApp.FCM,
    adapter: Pigeon.FCM,
    project_id: "example-project-123",
    service_account_json: File.read!("service-account.json")
  ```

  3. Start your dispatcher on application boot.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.FCM
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
        {YourApp.FCM, fcm_opts()}
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp fcm_opts do
      [
        adapter: Pigeon.FCM,
        project_id: "example-project-123",
        service_account_json: File.read!("service-account.json")
      ]
    end
  end
  ```

  4. Create a notification.

  ```
  n = Pigeon.FCM.Notification.new({:token, "reg ID"}, %{"body" => "test message"})
  ```
   
  5. Send the notification. 

  On successful response, `:name` will be set to the name returned from the FCM 
  API and `:response` will be `:success`. If there was an error, `:error` will 
  contain a JSON map of the response and `:response` will be an atomized version
  of the error type.

  ```
  YourApp.FCM.push(n)
  ```
  """

  @max_retries 3

  defstruct config: nil,
            queue: Pigeon.NotificationQueue.new(),
            refresh_before: 5 * 60,
            retries: @max_retries,
            socket: nil,
            stream_id: 1,
            token: nil

  @behaviour Pigeon.Adapter

  alias Pigeon.{Configurable, NotificationQueue}
  alias Pigeon.Http2.{Client, Stream}

  @refresh :"$refresh"
  @retry_after 1000

  @scopes [
    "https://www.googleapis.com/auth/cloud-platform",
    "https://www.googleapis.com/auth/firebase.messaging"
  ]

  @impl true
  def init(opts) do
    config = Pigeon.FCM.Config.new(opts)
    Configurable.validate!(config)

    state = %__MODULE__{config: config}

    with {:ok, socket} <- connect_socket(config),
         {:ok, token} <- fetch_token(config) do
      Configurable.schedule_ping(config)
      schedule_refresh(state, token)
      {:ok, %{state | socket: socket, token: token}}
    else
      {:error, reason} -> {:stop, reason}
    end
  end

  @impl true
  def handle_push(notification, state) do
    %{config: config, queue: queue, token: token} = state
    headers = Configurable.push_headers(config, notification, token: token)
    payload = Configurable.push_payload(config, notification, [])

    Client.default().send_request(state.socket, headers, payload)

    new_q = NotificationQueue.add(queue, state.stream_id, notification)

    state =
      state
      |> inc_stream_id()
      |> Map.put(:queue, new_q)

    {:noreply, state}
  end

  @impl true
  def handle_info(:ping, state) do
    Client.default().send_ping(state.socket)
    Configurable.schedule_ping(state.config)

    {:noreply, state}
  end

  def handle_info({:closed, _}, %{config: config} = state) do
    case connect_socket(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)

        state =
          state
          |> reset_stream_id()
          |> Map.put(:socket, socket)

        {:noreply, state}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  def handle_info(@refresh, %{config: config} = state) do
    case fetch_token(config) do
      {:ok, token} ->
        schedule_refresh(state, token)
        {:noreply, %{state | retries: @max_retries, token: token}}

      {:error, exception} ->
        if state.retries > 0 do
          Process.send_after(self(), @refresh, @retry_after)
          {:noreply, %{state | retries: state.retries - 1}}
        else
          raise "too many failed attempts to refresh, last error: #{inspect(exception)}"
        end
    end
  end

  def handle_info(msg, state) do
    case Client.default().handle_end_stream(msg, state) do
      {:ok, %Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  defp connect_socket(config), do: connect_socket(config, @max_retries)

  defp connect_socket(config, tries) do
    case Configurable.connect(config) do
      {:ok, socket} ->
        {:ok, socket}

      {:error, reason} ->
        if tries > 0 do
          connect_socket(config, tries - 1)
        else
          {:error, reason}
        end
    end
  end

  defp fetch_token(config) do
    source = {:service_account, config.service_account_json, [scopes: @scopes]}
    Goth.Token.fetch(%{source: source})
  end

  defp schedule_refresh(state, token) do
    time_in_seconds =
      max(token.expires - System.system_time(:second) - state.refresh_before, 0)

    Process.send_after(self(), @refresh, time_in_seconds * 1000)
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

  @doc false
  def reset_stream_id(state) do
    %{state | stream_id: 1}
  end
end
