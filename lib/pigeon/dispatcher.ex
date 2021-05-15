defmodule Pigeon.Dispatcher do
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

      def push(notification) do
        Pigeon.push(__MODULE__, notification)
      end

      def push(notification, opts) do
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
  def handle_cast({:push, notification, on_response}, %{adapter: adapter, state: state}) do
    state = adapter.handle_push(notification, on_response, state)
    {:noreply, %{adapter: adapter, state: state}}
  end

  @impl true
  def handle_info(msg, %{adapter: adapter, state: state}) do
    {:noreply, state} = adapter.handle_info(msg, state)
    {:noreply, %{adapter: adapter, state: state}}
  end
end
