defmodule Pigeon.FCM do
  @moduledoc """
  `Pigeon.Adapter` for Firebase Cloud Messaging (FCM) push notifications.

  ## Getting Started

  ### Create a dispatcher.

    ```
    # lib/your_app/fcm.ex

    defmodule YourApp.FCM do
      use Pigeon.Dispatcher, otp_app: :your_app
    end
    ```

  ### Install and configure Goth.

  Install and configure [`goth`](https://hexdocs.pm/goth/1.4.3/readme.html#installation)
  if you haven't already. `Pigeon.FCM` requires it for token authentication.

  ### Configure your dispatcher.

  Configure your `FCM` dispatcher and start it on application boot.

  ```
  # config.exs

  config :your_app, YourApp.FCM,
    adapter: Pigeon.FCM,
    auth: YourApp.Goth, # Your Goth worker configured in the previous step.
    project_id: "example-project-123"
  ```

  Add it to your supervision tree.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        {Goth, name: YourApp.Goth},
        YourApp.FCM
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
        {Goth, name: YourApp.Goth},
        {YourApp.FCM, fcm_opts()}
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp fcm_opts do
      [
        adapter: Pigeon.FCM,
        auth: YourApp.Goth,
        project_id: "example-project-123"
      ]
    end
  end
  ```

  ### Create a notification.

  ```
  n = Pigeon.FCM.Notification.new({:token, "reg ID"}, %{"body" => "test message"})
  ```

  ### Send the notification.

  On successful response, `:name` will be set to the name returned from the FCM
  API and `:response` will be `:success`. If there was an error, `:error` will
  contain a JSON map of the response and `:response` will be an atomized version
  of the error type.

  ```
  YourApp.FCM.push(n)
  ```

  ## Customizing Goth

  You can use any of the configuration options (e.g. `:source`) for Goth. Check out the 
  documentation of [`Goth.start_link/1`](https://hexdocs.pm/goth/Goth.html#start_link/1) 
  for more details.
  """

  @max_retries 3

  defstruct config: nil,
            queue: Pigeon.NotificationQueue.new(),
            retries: @max_retries,
            socket: nil,
            stream_id: 1

  @behaviour Pigeon.Adapter

  alias Pigeon.{Configurable, NotificationQueue}
  alias Pigeon.Http2.{Client, Stream}

  @impl Pigeon.Adapter
  def init(opts) do
    config = Pigeon.FCM.Config.new(opts)

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

  @impl Pigeon.Adapter
  def handle_push(notification, state) do
    %{config: config, queue: queue} = state
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

  @impl Pigeon.Adapter
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
