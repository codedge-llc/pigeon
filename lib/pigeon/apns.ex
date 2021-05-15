defmodule Pigeon.APNS do
  defstruct queue: Pigeon.NotificationQueue.new(),
            stream_id: 1,
            socket: nil,
            config: nil

  @behaviour Pigeon.Adapter

  alias Pigeon.{Configurable, NotificationQueue}
  alias Pigeon.Http2.{Client, Stream}

  @impl true
  def init(opts) do
    config = Pigeon.APNS.ConfigParser.parse(opts)
    Configurable.validate!(config)

    state = %__MODULE__{config: config}

    case connect_socket(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:ok, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_push(notification, on_response, %{config: config, queue: queue} = state) do
    headers = Configurable.push_headers(config, notification, [])
    payload = Configurable.push_payload(config, notification, [])

    Client.default().send_request(state.socket, headers, payload)

    new_q =
      NotificationQueue.add(
        queue,
        state.stream_id,
        notification,
        on_response
      )

    state
    |> inc_stream_id()
    |> Map.put(:queue, new_q)
  end

  def handle_info(:ping, state) do
    Client.default().send_ping(state.socket)
    Configurable.schedule_ping(state.config)

    {:noreply, state}
  end

  def handle_info({:closed, _}, %{config: config} = state) do
    case connect_socket(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:noreply, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_info(msg, state) do
    case Client.default().handle_end_stream(msg, state) do
      {:ok, %Stream{} = stream} -> process_end_stream(stream, state)
      _else -> {:noreply, state}
    end
  end

  defp connect_socket(config), do: connect_socket(config, 0)

  defp connect_socket(_config, 3), do: {:error, :timeout}

  defp connect_socket(config, tries) do
    case Configurable.connect(config) do
      {:ok, socket} -> {:ok, socket}
      {:error, _reason} -> connect_socket(config, tries + 1)
    end
  end

  def process_end_stream(%Stream{id: stream_id} = stream, state) do
    %{queue: queue, config: config} = state

    case NotificationQueue.pop(queue, stream_id) do
      {nil, new_queue} ->
        # Do nothing if no queued item for stream
        {:noreply, %{state | queue: new_queue}}

      {{notif, on_response}, new_queue} ->
        Configurable.handle_end_stream(config, stream, notif, on_response)
        {:noreply, %{state | queue: new_queue}}
    end
  end

  def inc_stream_id(%{stream_id: stream_id} = state) do
    %{state | stream_id: stream_id + 2}
  end
end
