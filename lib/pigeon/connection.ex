defmodule Pigeon.Connection do
  @moduledoc false

  alias Pigeon.Connection.NotificationQueue

  defstruct completed: 0,
            config: nil,
            from: nil,
            requested: 0,
            socket: nil,
            stream_id: 1,
            queue: NotificationQueue.new()

  use GenStage
  require Logger

  alias Pigeon.{Configurable, Connection}
  alias Pigeon.Http2.{Client, Stream}

  @limit 1_000_000_000

  def handle_subscribe(:producer, _opts, from, state) do
    demand = Configurable.max_demand(state.config)
    GenStage.ask(from, demand)

    state =
      state
      |> inc_requested(demand)
      |> Map.put(:from, from)

    {:manual, state}
  end

  def start_link({config, from}) do
    GenStage.start_link(__MODULE__, {config, from})
  end

  def init({config, from}) do
    state = %Connection{config: config, from: from}

    case connect_socket(config, 0) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:consumer, %{state | socket: socket}, subscribe_to: [from]}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  def connect_socket(_config, 3), do: {:error, :timeout}

  def connect_socket(config, tries) do
    case Configurable.connect(config) do
      {:ok, socket} -> {:ok, socket}
      {:error, _reason} -> connect_socket(config, tries + 1)
    end
  end

  # Handle Cancels

  def handle_cancel({:down, _}, _from, state) do
    {:stop, :normal, state}
  end

  def handle_cancel({:cancel, :closed}, _from, state) do
    {:stop, :normal, state}
  end

  def handle_cancel({:cancel, :stream_id_exhausted}, _from, state) do
    {:stop, :normal, state}
  end

  # Info

  def handle_info(:ping, state) do
    Client.default().send_ping(state.socket)
    Configurable.schedule_ping(state.config)

    {:noreply, [], state}
  end

  def handle_info({:closed, _}, %{from: from} = state) do
    GenStage.cancel(from, :closed)
    {:noreply, [], %{state | socket: nil}}
  end

  def handle_info(msg, state) do
    case Client.default().handle_end_stream(msg, state) do
      {:ok, %Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, [], state}
    end
  end

  def handle_events(events, _from, state) do
    state =
      Enum.reduce(events, state, fn {:push, notif, opts}, state ->
        send_push(state, notif, opts)
      end)

    {:noreply, [], state}
  end

  def process_end_stream(%Stream{id: stream_id} = stream, state) do
    %{queue: queue, config: config} = state

    case NotificationQueue.pop(queue, stream_id) do
      {nil, new_queue} ->
        # Do nothing if no queued item for stream
        {:noreply, [], %{state | queue: new_queue}}

      {{notif, on_response}, new_queue} ->
        Configurable.handle_end_stream(config, stream, notif, on_response)

        state =
          state
          |> inc_completed(1)
          |> dec_requested(1)
          |> Map.put(:queue, new_queue)

        total_requests = state.completed + state.requested
        max_demand = Configurable.max_demand(state.config)

        state =
          if total_requests < @limit and state.requested < max_demand do
            to_ask = min(@limit - total_requests, max_demand - state.requested)
            GenStage.ask(state.from, to_ask)
            inc_requested(state, to_ask)
          else
            state
          end

        if state.completed >= @limit do
          GenStage.cancel(state.from, :stream_id_exhausted)
        end

        {:noreply, [], state}
    end
  end

  def send_push(%{config: config, queue: queue} = state, notification, opts) do
    headers = Configurable.push_headers(config, notification, opts)
    payload = Configurable.push_payload(config, notification, opts)

    Client.default().send_request(state.socket, headers, payload)

    new_q =
      NotificationQueue.add(
        queue,
        state.stream_id,
        notification,
        opts[:on_response]
      )

    state
    |> inc_stream_id()
    |> Map.put(:queue, new_q)
  end

  # Cast

  def handle_cast(_msg, state) do
    {:noreply, [], state}
  end

  # Helpers

  def inc_requested(state, count) do
    %{state | requested: state.requested + count}
  end

  def dec_requested(state, count) do
    %{state | requested: state.requested - count}
  end

  def inc_completed(state, count) do
    %{state | completed: state.completed + count}
  end

  def inc_stream_id(%{stream_id: stream_id} = state) do
    %{state | stream_id: stream_id + 2}
  end
end
