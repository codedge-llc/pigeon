defmodule Pigeon.Worker do
  @moduledoc false

  defstruct config: nil,
            connections: 0,
            pending_demand: 0,
            queue: :queue.new()

  use GenStage

  alias Pigeon.{APNS, Configurable, FCM, Worker}

  require Logger

  @type config :: APNS.Config.t() | FCM.Config.t()

  @type notification ::
          APNS.Notification.t()
          | FCM.Notification.t()

  @spec start_link(config) :: {:ok, pid}
  def start_link(config) do
    case Configurable.worker_name(config) do
      nil -> GenStage.start_link(__MODULE__, {:ok, config})
      name -> GenStage.start_link(__MODULE__, {:ok, config}, name: name)
    end
  end

  @spec stop_connection(pid) :: :ok
  def stop_connection(pid) do
    GenStage.cast(pid, :stop)
  end

  @spec send_push(atom | pid, notification, Keyword.t()) :: :ok
  def send_push(name, notification, opts) do
    # Ensure connections are live before trying to push
    # Doesn't play nice if you try to do it all in one step
    timeout = Keyword.get(opts, :timeout, 5_000)
    GenStage.call(name, :ensure_connection, timeout)
    GenStage.call(name, {:push, notification, opts}, timeout)
  end

  def init({:ok, config}) do
    state = %Worker{config: config}
    {:producer, state}
  end

  def handle_call(:ensure_connection, _from, state) do
    {:reply, :ok, [], ensure_connections(state)}
  end

  def handle_call({:push, _notif, _opts} = msg, from, state) do
    GenStage.reply(from, :ok)

    state
    |> Map.update(:queue, :queue.new(), &:queue.in(msg, &1))
    |> dispatch_events([])
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_demand(incoming_demand, state) do
    state
    |> increment_demand(incoming_demand)
    |> dispatch_events([])
  end

  def handle_cancel({:cancel, :stream_id_exhausted}, _from, state) do
    Pigeon.start_connection({state.config, self()})

    {:noreply, [], state}
  end

  def handle_cancel({:cancel, :closed}, _from, state) do
    state = Map.update(state, :connections, 0, &(&1 - 1))
    {:noreply, [], state}
  end

  def handle_cancel({:down, _error}, _from, state) do
    state = %{state | connections: state.connections - 1}
    {:noreply, [], state}
  end

  def handle_info(_msg, state) do
    {:noreply, [], state}
  end

  defp ensure_connections(%{connections: c} = state) when c <= 0 do
    Pigeon.start_connection({state.config, self()})
    %{state | connections: state.connections + 1}
  end

  defp ensure_connections(state), do: state

  defp dispatch_events(%{pending_demand: 0} = state, events) do
    {:noreply, Enum.reverse(events), state}
  end

  defp dispatch_events(%{queue: queue} = state, events) do
    case :queue.out(queue) do
      {{:value, event}, queue} ->
        state
        |> Map.put(:queue, queue)
        |> increment_demand(-1)
        |> dispatch_events([event | events])

      {:empty, _queue} ->
        {:noreply, Enum.reverse(events), state}
    end
  end

  defp increment_demand(%{pending_demand: d} = state, amount) do
    Map.update(state, :pending_demand, d, &(&1 + amount))
  end
end
