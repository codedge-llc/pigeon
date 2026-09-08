defmodule Pigeon.HTTP.Connection do
  @moduledoc false

  # Connection state machine shared by the APNS, FCM, and ADM adapters.
  #
  # The struct is pure data that the owning worker process threads through
  # its callbacks. Functions here run inside that process, so they can send
  # `:connect` and `:ping` messages to `self()` and can complete notifications
  # through `Pigeon.Tasks`.
  #
  # Every path that loses the socket ends in `disconnect/2`. Nothing else
  # decides what to do with a dead connection.

  alias Pigeon.HTTP.{Request, RequestQueue}
  alias Pigeon.Tasks

  require Logger

  @initial_backoff 500
  @max_backoff 30_000

  defstruct adapter: nil,
            connect: nil,
            ping_period: nil,
            socket: nil,
            queue: RequestQueue.new(),
            status: :disconnected,
            attempt: 0,
            ping_timer: nil

  @type status :: :disconnected | :connected | :draining
  @type connect_fun :: (-> {:ok, Mint.HTTP.t()} | {:error, term()})
  @type handler :: (Request.t() -> any())

  @type t :: %__MODULE__{
          adapter: module(),
          connect: connect_fun(),
          ping_period: pos_integer() | nil,
          socket: Mint.HTTP.t() | nil,
          queue: RequestQueue.t(),
          status: status(),
          attempt: non_neg_integer(),
          ping_timer: reference() | nil
        }

  # Builds a disconnected connection and queues the first `:connect` message.
  #
  # `adapter` names the owning adapter in log lines. `connect_fun` opens the
  # socket. `ping_period` is the keepalive interval in milliseconds, or `nil`
  # for no keepalive.
  @spec new(module(), connect_fun(), pos_integer() | nil) :: t()
  def new(adapter, connect_fun, ping_period \\ nil) do
    send(self(), :connect)

    %__MODULE__{
      adapter: adapter,
      connect: connect_fun,
      ping_period: ping_period
    }
  end

  @spec connected?(t()) :: boolean()
  def connected?(%__MODULE__{status: :connected}), do: true
  def connected?(%__MODULE__{}), do: false

  # Routes a worker message to the matching connection function.
  #
  # Returns `:unknown` for messages that do not belong to this connection.
  # Completed requests with a response are passed to `handler`. Requests that
  # failed before a response arrived are completed here and never reach the
  # handler.
  @spec handle_message(t(), term(), handler()) :: {:ok, t()} | :unknown
  def handle_message(conn, :connect, _handler), do: {:ok, connect(conn)}
  def handle_message(conn, :ping, _handler), do: {:ok, ping(conn)}

  def handle_message(conn, message, handler) do
    case stream(conn, message) do
      :unknown ->
        :unknown

      {:ok, conn, done} ->
        Enum.each(done, handler)
        {:ok, conn}
    end
  end

  # Attempts to open the socket. Ignored unless the connection is disconnected.
  #
  # On failure, logs at `:error` and schedules the next `:connect` with
  # exponential backoff and jitter.
  @spec connect(t()) :: t()
  def connect(%__MODULE__{status: :disconnected} = conn) do
    case conn.connect.() do
      {:ok, socket} ->
        schedule_ping(%{conn | socket: socket, status: :connected, attempt: 0})

      {:error, reason} ->
        Logger.error("#{prefix(conn)} failed to connect: #{format(reason)}")
        attempt = conn.attempt + 1
        Process.send_after(self(), :connect, backoff(attempt))
        %{conn | attempt: attempt}
    end
  end

  def connect(%__MODULE__{} = conn), do: conn

  # Sends a request and tracks it in the queue.
  #
  # When there is no live connection or the request is rejected, nothing is
  # sent and the notification is completed with `:not_connected`. A rejection
  # that means the socket is dead also disconnects.
  @spec request(
          t(),
          String.t(),
          String.t(),
          Mint.Types.headers(),
          iodata(),
          term()
        ) :: t()
  def request(conn, method, path, headers, body, notification) do
    case do_request(conn, method, path, headers, body, notification) do
      {:ok, conn, _ref} ->
        conn

      {:error, conn, _reason} ->
        fail_request(Request.new(notification), :not_connected)
        conn
    end
  end

  # Sends a request and blocks until its response arrives or `timeout` elapses.
  #
  # For adapters that need a synchronous exchange, such as a token refresh.
  # Only messages for this socket are consumed, so other worker messages stay
  # in the mailbox. Responses for other in-flight requests that arrive while
  # waiting are kept in the queue and delivered on the next `stream/2`.
  @spec request_sync(
          t(),
          String.t(),
          String.t(),
          Mint.Types.headers(),
          iodata(),
          timeout()
        ) ::
          {:ok, t(), Request.t()} | {:error, t(), term()}
  def request_sync(conn, method, path, headers, body, timeout \\ 5_000) do
    case do_request(conn, method, path, headers, body, nil) do
      {:ok, conn, ref} ->
        deadline = System.monotonic_time(:millisecond) + timeout
        await(conn, ref, deadline)

      {:error, conn, reason} ->
        {:error, conn, reason}
    end
  end

  # Sends an HTTP/2 PING and reschedules the next one. Ignored unless connected.
  @spec ping(t()) :: t()
  def ping(%__MODULE__{status: :connected, socket: socket} = conn) do
    conn = %{conn | ping_timer: nil}

    if Mint.HTTP.protocol(socket) == :http2 do
      case Mint.HTTP2.ping(socket) do
        {:ok, socket, _ref} -> schedule_ping(%{conn | socket: socket})
        {:error, socket, error} -> disconnect(%{conn | socket: socket}, error)
      end
    else
      conn
    end
  end

  def ping(%__MODULE__{} = conn), do: %{conn | ping_timer: nil}

  # Feeds a socket message to Mint.
  #
  # Returns `:unknown` if the message is not for this socket. Otherwise returns
  # the requests that completed with a response. Requests that completed with
  # an error are failed here through `Pigeon.Tasks`.
  @spec stream(t(), term()) :: :unknown | {:ok, t(), [Request.t()]}
  def stream(%__MODULE__{socket: nil}, _message), do: :unknown

  def stream(%__MODULE__{socket: socket} = conn, message) do
    case Mint.HTTP.stream(socket, message) do
      :unknown ->
        :unknown

      {:ok, socket, responses} ->
        {done, conn} = process_responses(%{conn | socket: socket}, responses)
        {:ok, check_writable(conn), done}

      {:error, socket, error, responses} ->
        {done, conn} = process_responses(%{conn | socket: socket}, responses)
        {:ok, disconnect(conn, error), done}
    end
  end

  # Drops the socket, fails every in-flight notification with `:timeout`, and
  # queues an immediate `:connect`.
  @spec disconnect(t(), term()) :: t()
  def disconnect(%__MODULE__{} = conn, reason) do
    Logger.error("#{prefix(conn)} disconnected: #{format(reason)}")
    drop_socket(conn)
  end

  # Private

  # Shared by `disconnect/2` and the GOAWAY path. A GOAWAY with NO_ERROR is
  # routine, FCM sends one to every connection after a few minutes, so it is
  # not logged at all. Mint surfaces a GOAWAY with any other code as a stream
  # error, which reaches `disconnect/2` and logs at `:error`.
  defp drop_socket(conn) do
    close_socket(conn.socket)
    cancel_ping(conn.ping_timer)

    {requests, queue} = RequestQueue.drain(conn.queue)
    Enum.each(requests, &fail_request(&1, :timeout))

    send(self(), :connect)

    %{
      conn
      | socket: nil,
        queue: queue,
        status: :disconnected,
        attempt: 0,
        ping_timer: nil
    }
  end

  defp do_request(
         %{status: :connected} = conn,
         method,
         path,
         headers,
         body,
         notification
       ) do
    case Mint.HTTP.request(conn.socket, method, path, headers, body) do
      {:ok, socket, ref} ->
        queue = RequestQueue.add(conn.queue, ref, notification)
        {:ok, %{conn | socket: socket, queue: queue}, ref}

      {:error, socket, %Mint.HTTPError{reason: :closed_for_writing} = error} ->
        {:error, handle_goaway(%{conn | socket: socket}), error}

      {:error, socket, %Mint.HTTPError{reason: :closed} = error} ->
        {:error, disconnect(%{conn | socket: socket}, error), error}

      {:error, socket, %Mint.TransportError{} = error} ->
        {:error, disconnect(%{conn | socket: socket}, error), error}

      {:error, socket, error} ->
        Logger.warning("#{prefix(conn)} request rejected: #{format(error)}")
        {:error, %{conn | socket: socket}, error}
    end
  end

  defp do_request(conn, _method, _path, _headers, _body, _notification) do
    {:error, conn, :not_connected}
  end

  defp await(conn, ref, deadline) do
    remaining = deadline - System.monotonic_time(:millisecond)
    socket = Mint.HTTP.get_socket(conn.socket)

    receive do
      {tag, ^socket, _data} = message
      when tag in [:ssl, :tcp, :ssl_error, :tcp_error] ->
        await_stream(conn, ref, deadline, message)

      {tag, ^socket} = message when tag in [:ssl_closed, :tcp_closed] ->
        await_stream(conn, ref, deadline, message)
    after
      max(remaining, 0) ->
        {_request, queue} = RequestQueue.pop(conn.queue, ref)
        {:error, %{conn | queue: queue}, :timeout}
    end
  end

  defp await_stream(conn, ref, deadline, message) do
    case Mint.HTTP.stream(conn.socket, message) do
      :unknown ->
        await(conn, ref, deadline)

      {:ok, socket, responses} ->
        queue = RequestQueue.process(responses, conn.queue)
        conn = %{conn | socket: socket, queue: queue}

        case queue.requests[ref] do
          %Request{done?: true, error: nil} = request ->
            {_request, queue} = RequestQueue.pop(queue, ref)
            {:ok, check_writable(%{conn | queue: queue}), request}

          %Request{done?: true, error: error} ->
            {_request, queue} = RequestQueue.pop(queue, ref)
            {:error, check_writable(%{conn | queue: queue}), error}

          _not_done ->
            await(conn, ref, deadline)
        end

      {:error, socket, error, responses} ->
        {_done, conn} = process_responses(%{conn | socket: socket}, responses)
        {:error, disconnect(conn, error), error}
    end
  end

  defp process_responses(conn, responses) do
    {done, queue} =
      responses
      |> RequestQueue.process(conn.queue)
      |> RequestQueue.pop_done()

    {delivered, failed} =
      done
      |> Enum.map(fn {_ref, request} -> request end)
      |> Enum.reject(&is_nil(&1.notification))
      |> Enum.split_with(&is_nil(&1.error))

    Enum.each(failed, &fail_request(&1, stream_error_response(&1.error)))

    {delivered, %{conn | queue: queue}}
  end

  # The server refused the stream before processing it (RFC 9113 GOAWAY), so
  # the push was never sent. Any other stream error leaves delivery unknown.
  defp stream_error_response(%Mint.HTTPError{reason: :unprocessed}),
    do: :not_connected

  defp stream_error_response(_error), do: :timeout

  # After a GOAWAY the socket still delivers responses for in-flight requests
  # but accepts no new ones. Drain those first, then reconnect.
  #
  # HTTP/2 only. Mint's HTTP/1 `open?/2` reports false while it parses a
  # response, which is not a closed connection.
  defp check_writable(%{status: status, socket: socket} = conn)
       when status in [:connected, :draining] do
    cond do
      Mint.HTTP.protocol(socket) != :http2 -> conn
      Mint.HTTP.open?(socket, :write) -> conn
      true -> handle_goaway(conn)
    end
  end

  defp check_writable(conn), do: conn

  defp handle_goaway(conn) do
    if RequestQueue.empty?(conn.queue) do
      drop_socket(conn)
    else
      %{conn | status: :draining}
    end
  end

  defp fail_request(%Request{notification: nil}, _response), do: :ok

  defp fail_request(%Request{notification: notification}, response) do
    notification |> Map.put(:response, response) |> Tasks.process_on_response()
  end

  defp schedule_ping(%{ping_period: nil} = conn), do: conn

  defp schedule_ping(%{ping_period: period} = conn) do
    cancel_ping(conn.ping_timer)
    %{conn | ping_timer: Process.send_after(self(), :ping, period)}
  end

  defp cancel_ping(nil), do: :ok
  defp cancel_ping(timer), do: Process.cancel_timer(timer)

  defp close_socket(nil), do: :ok
  defp close_socket(socket), do: Mint.HTTP.close(socket)

  defp backoff(attempt) do
    base = min(@initial_backoff * Integer.pow(2, attempt - 1), @max_backoff)
    jitter = :rand.uniform(div(base, 2) + 1) - div(base, 4) - 1
    max(base + jitter, 0)
  end

  defp prefix(%{adapter: adapter}),
    do: "#{inspect(self())} (#{inspect(adapter)})"

  defp format(reason) when is_exception(reason),
    do: Exception.format(:error, reason)

  defp format(reason), do: inspect(reason)
end
