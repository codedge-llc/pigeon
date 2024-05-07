defmodule Pigeon.DispatcherWorker do
  @moduledoc false

  use GenServer

  def start_link(opts) do
    opts[:adapter] || raise "adapter is not specified"
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    case opts[:adapter].init(opts) do
      {:ok, state} ->
        Pigeon.Registry.register(opts[:supervisor])
        {:ok, %{adapter: opts[:adapter], state: state}}

      {:error, reason} ->
        {:error, reason}

      {:stop, reason} ->
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_info({:"$push", notification}, %{adapter: adapter, state: state}) do
    case adapter.handle_push(notification, state) do
      {:noreply, new_state} ->
        {:noreply, %{adapter: adapter, state: new_state}}

      {:stop, reason, new_state} ->
        {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end

  def handle_info(msg, %{adapter: adapter, state: state}) do
    case adapter.handle_info(msg, state) do
      {:noreply, new_state} ->
        {:noreply, %{adapter: adapter, state: new_state}}

      {:stop, reason, new_state} ->
        {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end

  @impl GenServer
  def handle_call(:info, _from, %{adapter: adapter, state: state}) do
    info = %{peername: peername(state)}
    {:reply, info, %{adapter: adapter, state: state}}
  end

  defp peername(state) do
    with %{socket: socket} <- state,
         %{connection: connection} <- :sys.get_state(socket),
         %{config: %{socket: socket2}} <- :sys.get_state(connection),
         %{socket: socket3} <- :sys.get_state(socket2),
         {_, {_, port, _, _}, _} <- socket3,
         {:ok, addr} <- :inet.peername(port) do
      addr
    else
      _ -> "unknown"
    end
  end
end
