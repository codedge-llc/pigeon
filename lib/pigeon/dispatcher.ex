defmodule Pigeon.Dispatcher do
  defstruct adapter: nil, state: nil

  use GenServer

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @adapter opts[:adapter]

      def child_spec(config) do
        opts = [[adapter: @adapter, config: config, name: __MODULE__]]

        %{
          id: __MODULE__,
          start: {Pigeon.Dispatcher, :start_link, opts},
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
    opts[:config] || raise "config is not specified"
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  @impl true
  def init(opts) do
    state = opts[:adapter].initial_state(opts[:config])

    {:ok, %__MODULE__{adapter: opts[:adapter], state: state}}
  end

  @impl true
  def handle_cast({:push, notification, on_response}, %{adapter: adapter, state: state}) do
    state = adapter.handle_push(notification, on_response, state)
    {:noreply, %__MODULE__{adapter: adapter, state: state}}
  end

  @impl true
  def handle_info(msg, %{adapter: adapter, state: state}) do
    {:noreply, state} = adapter.handle_info(msg, state)
    {:noreply, %__MODULE__{adapter: adapter, state: state}}
  end
end
