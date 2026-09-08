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

  Install and configure [`goth`](https://hexdocs.pm/goth/readme.html#installation)
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

  If the dispatcher has no live connection to FCM, `:response` is `:not_connected`
  and the push was not sent, so it is safe to resend. If the connection is lost after
  the push was sent, `:response` is `:timeout` and delivery is unknown.

  ```
  YourApp.FCM.push(n)
  ```

  ## Configuration Options

  - `:auth` - Your Goth worker name or module. Required.
  - `:project_id` - Your Firebase project ID. Required.
  - `:ping_period` - Interval between server pings in milliseconds. Keeps idle
    connections alive. Defaults to 10 minutes.
  - `:port` - Push server port. Defaults to `443`.
  - `:uri` - Push server uri. Defaults to `fcm.googleapis.com`. Useful for test
    environments.

  ## Customizing Goth

  You can use any of the configuration options (e.g. `:source`) for Goth. Check out the
  documentation of [`Goth.start_link/1`](https://hexdocs.pm/goth/Goth.html#start_link/1)
  for more details.
  """

  defstruct config: nil,
            conn: nil

  @behaviour Pigeon.Adapter

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Configurable
  alias Pigeon.FCM.Error
  alias Pigeon.HTTP.{Connection, Request}

  @impl Pigeon.Adapter
  def init(opts) do
    config = Pigeon.FCM.Config.new(opts)

    Configurable.validate!(config)

    conn =
      Connection.new(
        __MODULE__,
        fn -> Configurable.connect(config) end,
        Configurable.ping_period(config)
      )

    {:ok, %__MODULE__{config: config, conn: conn}}
  end

  @impl Pigeon.Adapter
  def handle_push(notification, %{config: config, conn: conn} = state) do
    headers = Configurable.push_headers(config, notification, [])
    payload = Configurable.push_payload(config, notification, [])
    path = "/v1/projects/#{config.project_id}/messages:send"

    conn =
      Connection.request(conn, "POST", path, headers, payload, notification)

    {:noreply, %{state | conn: conn}}
  end

  @impl Pigeon.Adapter
  def handle_info(msg, %{conn: conn} = state) do
    case Connection.handle_message(conn, msg, &handle_response/1) do
      {:ok, conn} -> {:noreply, %{state | conn: conn}}
      :unknown -> {:noreply, state}
    end
  end

  @doc false
  @spec handle_response(Request.t()) :: :ok
  def handle_response(%{body: body, notification: notif}) do
    body
    |> Pigeon.json_library().decode!()
    |> case do
      %{"name" => name} ->
        notif
        |> Map.put(:name, name)
        |> Map.put(:response, :success)
        |> process_on_response()

      %{"error" => error} ->
        notif
        |> Map.put(:error, error)
        |> Map.put(:response, Error.parse(error))
        |> process_on_response()
    end
  end
end
