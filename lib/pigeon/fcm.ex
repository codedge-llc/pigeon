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

  2. Configure the [`goth`](https://hexdocs.pm/goth/1.4.3/readme.html#installation) library, and add it to `config.exs`

  ```
  # config.exs
  # See Step 3 for alternative configuration

  config :your_app, YourApp.FCM,
    adapter: Pigeon.FCM,
    project_id: "example-project-123",
    token_fetcher: YourApp.Goth
  ```

  3. Start your dispatcher on application boot.

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

  If preferred, you can include your configuration directly

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
        project_id: "example-project-123",
        token_fetcher: YourApp.Goth
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

  ## Customizable Goth Token Fetcher
  If you need a customizable `:token_fetcher` that handles fetching its own configuration, here's
  an example you can use to get started.

  For other `:source` configurations of `YourApp.Goth`, check out the `goth` documentation for [`Goth.start_link/1`](https://hexdocs.pm/goth/Goth.html#start_link/1)

  ```
  # lib/your_app/goth.ex
  defmodule YourApp.Goth

    @spec child_spec(any()) :: Supervisor.child_spec()
    def child_spec(_args) do
      env_opts = Keyword.new(Application.get_env(:your_app, YourApp.Goth, []))
      opts = Keyword.merge([name: YourApp.Goth], env_opts)

      %{
        :id => YourApp.Goth,
        :start => {Goth, :start_link, [opts]}
      }
    end
  end

  # config.exs
  config :your_app, YourApp.Goth, source: {:metadata, []}

  # config/test.exs
  config :your_app, YourApp.Goth,
    source: {:metadata, []},
    http_client: {&PigeonTest.GothHttpClient.Stub.access_token_response/1, []}

  # application.exs
  def start(_type, _args) do
    children = [
      # The `child_spec/1` handles fetching the proper config
      YourApp.Goth,
      YourApp.FCM
    ]
    opts = [strategy: :one_for_one, name: YourApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
  ```

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
