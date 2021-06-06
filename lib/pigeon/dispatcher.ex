defmodule Pigeon.Dispatcher do
  @moduledoc """
  Dispatcher worker for push notifications.

  If your push workers are relatively static, it is encouraged to follow the adapter
  guides. For other use cases, such as supporting dynamic configurations, dispatchers
  can be started and stopped as needed.

  ## Using Dynamic Dispatchers

  ```
  # FCM as an example, but use the relevant options for your push type.
  opts = [
    adapter: Pigeon.FCM,
    project_id: "example-project-123",
    service_account_json: File.read!("service-account.json")
  ]

  {:ok, pid} = Pigeon.Dispatcher.start_link(opts)
  notification = Pigeon.FCM.Notification.new({:token, "regid"})

  Pigeon.push(pid, notification)
  ```

  ## Loading Configurations from a Database

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.Repo,
        {Registry, keys: :unique, name: Registry.YourApp}
      ] ++ push_workers()
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp push_workers do
      YourApp.Repo.PushApplication
      |> YourApp.Repo.all()
      |> Enum.map(&push_spec/1)
    end

    defp push_spec(%{type: "apns"} = config)
      {Pigeon.Dispatcher, [
        adapter: Pigeon.APNS,
        key: config.key,
        key_identifier: config.key_identifier,
        team_id: config.team_id,
        mode: config.mode,
        name: {:via, Registry, {Registry.YourApp, config.name}}
      ]}
    end

    defp push_spec(%{type: "fcm"} = config) do
      {Pigeon.Dispatcher, [
        adapter: Pigeon.FCM,
        name: {:via, Registry, {Registry.YourApp, config.name}},
        project_id: config.project_id,
        service_account_json: config.service_account_json
      ]}
    end
  end
  ```

  Once running, you can send to any of these workers by name.

  ```
  Pigeon.push({:via, Registry, {Registry.YourApp, "app1"}}, notification)
  ```
  """

  use GenServer

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @otp_app opts[:otp_app]

      def child_spec(opts \\ []) do
        config_opts = Application.get_env(@otp_app, __MODULE__, [])

        opts =
          [name: __MODULE__]
          |> Keyword.merge(config_opts)
          |> Keyword.merge(opts)

        %{
          id: __MODULE__,
          start: {Pigeon.Dispatcher, :start_link, [opts]},
          type: :worker
        }
      end

      @doc """
      Sends a push notification with given options.
      """
      def push(notification, opts \\ []) do
        Pigeon.push(__MODULE__, notification, opts)
      end
    end
  end

  def start_link(opts) do
    opts[:adapter] || raise "adapter is not specified"
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  @impl true
  def init(opts) do
    case opts[:adapter].init(opts) do
      {:ok, state} ->
        {:ok, %{adapter: opts[:adapter], state: state}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def handle_cast({:push, notification}, %{adapter: adapter, state: state}) do
    case adapter.handle_push(notification, state) do
      {:noreply, new_state} -> {:noreply, %{adapter: adapter, state: new_state}}
      {:stop, reason, new_state} -> {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end

  @impl true
  def handle_info(msg, %{adapter: adapter, state: state}) do
    case adapter.handle_info(msg, state) do
      {:noreply, new_state} -> {:noreply, %{adapter: adapter, state: new_state}}
      {:stop, reason, new_state} -> {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end
end
