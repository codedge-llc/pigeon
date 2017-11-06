defmodule Pigeon.Worker do
  @moduledoc false

  defstruct config: nil, connections: 0

  use GenStage

  alias Pigeon.{Configurable, Connection, Worker}

  def start_link(config) do
    case Configurable.worker_name(config) do
      nil -> GenStage.start_link(__MODULE__, {:ok, config})
      name -> GenStage.start_link(__MODULE__, {:ok, config}, name: name)
    end
  end

  def stop_connection(pid) do
    GenStage.cast(pid, :stop)
  end

  def send_push(name, notification, opts) do
    GenStage.call(name, {:push, notification, opts}, 5000)
  end

  def init({:ok, config}) do
    Connection.start_link({config, self()})
    {:producer, %Worker{config: config, connections: 1}}
  end

  def handle_call({:push, _notification, _opts} = msg, _from, state) do
    state =
      if state.connections <= 0 do
        Connection.start_link({state.config, self()})
        %{state | connections: state.connections + 1}
      else
        state
      end
    {:reply, :ok, [msg], state} # Dispatch immediately
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_demand(_demand, state) do
    {:noreply, [], state}
  end

  def handle_cancel({:cancel, :stream_id_exhausted}, _from, state) do
    Connection.start_link({state.config, self()})
    {:noreply, [], state}
  end

  def handle_cancel({:cancel, :closed}, _from, state) do
    state = %{state | connections: state.connections - 1}
    {:noreply, [], state}
  end
end
