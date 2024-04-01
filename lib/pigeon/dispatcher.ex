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
    token_fetcher: YourApp.Goth
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
        {Goth, name: YourApp.Goth},
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
        token_fetcher: String.to_existing_atom(config.token_fetcher)
      ]}
    end
  end
  ```

  Once running, you can send to any of these workers by name.

  ```
  Pigeon.push({:via, Registry, {Registry.YourApp, "app1"}}, notification)
  ```
  """

  use Supervisor

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @otp_app opts[:otp_app]

      def child_spec(opts \\ []) do
        config_opts = Application.get_env(@otp_app, __MODULE__, [])

        opts =
          [name: __MODULE__, pool_size: Pigeon.default_pool_size()]
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
    Supervisor.start_link(__MODULE__, opts, name: opts[:name])
  end

  def init(opts) do
    opts =
      opts
      |> Keyword.put(:supervisor, opts[:name] || self())
      |> Keyword.delete(:name)

    children =
      for index <- 1..(opts[:pool_size] || Pigeon.default_pool_size()) do
        Supervisor.child_spec({Pigeon.DispatcherWorker, opts}, id: index)
      end

    Supervisor.init(children, strategy: :one_for_one)
  end
end
