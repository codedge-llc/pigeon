defmodule Pigeon.Worker do
  @moduledoc """
  Generic HTTP2 worker.
  """
  defstruct [:socket, :config, queue: %{}, stream_id: 1]

  use GenServer
  require Logger

  alias Pigeon.Configurable
  alias Pigeon.Http2.Stream
  alias Pigeon.Worker.NotificationQueue

  def cast_push(pid, notification, opts) do
    GenServer.cast(pid, {:push, notification, opts})
  end

  def start_link(config) do
    case Configurable.worker_name(config) do
      nil -> GenServer.start_link(__MODULE__, {:ok, config})
      name -> GenServer.start_link(__MODULE__, {:ok, config}, name: name)
    end
  end

  def stop_connection(pid) do
    GenServer.cast(pid, :stop)
  end

  def stop, do: :gen_server.cast(self(), :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    case connect_socket(config) do
      {:ok, socket} ->
        Process.send_after(self(), :ping, Configurable.ping_period(config))
        {:ok, %{
          socket: socket,
          config: config,
          stream_id: 1,
          queue: %{}
        }}
      error -> error
    end
  end

  def connect_socket(config), do: connect_socket(config, 0)

  def connect_socket(_config, 3), do: {:error, :timeout}
  def connect_socket(config, tries) do
    case Configurable.connect(config) do
      {:ok, socket} -> {:ok, socket}
      {:error, reason} ->
        IO.inspect reason
        connect_socket(config, tries + 1)
    end
  end

  # Info

  def handle_info(:ping, state) do
    Pigeon.Http2.Client.default().send_ping(state.socket)
    Process.send_after(self(), :ping, Configurable.ping_period(state.config))

    {:noreply, state}
  end

  def handle_info({:closed, _}, %{config: config} = state) do
    if Configurable.reconnect?(config) do
      {:noreply, reconnect(state)}
    else
      {:noreply, %{state | socket: nil}}
    end
  end

  def handle_info(msg, state) do
    case Pigeon.Http2.Client.default().handle_end_stream(msg, state) do
      {:ok, %Pigeon.Http2.Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  def process_end_stream(%Stream{id: stream_id} = stream,
                         %{queue: queue, config: config} = state) do
    case NotificationQueue.pop(queue, stream_id) do
      {nil, new_queue} ->
        # Do nothing if no queued item for stream
        {:noreply, %{state | queue: new_queue}}
      {{notification, on_response}, new_queue} ->
        Configurable.handle_end_stream(config, stream, notification, on_response)
        {:noreply, %{state | queue: new_queue}}
    end
  end

  def send_push(%{socket: socket,
                  config: config,
                  queue: queue} = state, notification, opts) do

    state =
      case socket do
        nil ->
          reconnect(state)
        _socket ->
          state
      end

    headers = Configurable.push_headers(config, notification, opts)
    payload = Configurable.push_payload(config, notification, opts)

    Pigeon.Http2.Client.default().send_request(state.socket, headers, payload)

    new_q = NotificationQueue.add(queue, state.stream_id, notification, opts[:on_response])
    new_stream_id = state.stream_id + 2

    {:noreply, %{state | stream_id: new_stream_id, queue: new_q}}
  end

  def reconnect(%{config: config} = state) do
    case connect_socket(config) do
      {:ok, new_socket} ->
        Process.send_after(self(), :ping, Configurable.ping_period(config))
        %{state | socket: new_socket, queue: %{}, stream_id: 1}
      error ->
        error |> inspect() |> Logger.error
        state
    end
  end

  # Cast

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, notification, opts}, state) do
    send_push(state, notification, opts)
  end

  def handle_cast(msg, state) do
    Logger.debug "Recv: #{inspect(msg)}"
    {:noreply, state}
  end
end
