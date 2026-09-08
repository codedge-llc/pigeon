defmodule Pigeon.HTTP.ConnectionTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Pigeon.HTTP.{Connection, Request}
  alias PigeonTest.Server

  @socket_tags [:ssl, :ssl_closed, :ssl_error]

  setup do
    {:ok, server, port} = Server.start()
    %{server: server, port: port}
  end

  describe "new/3" do
    test "starts disconnected and queues the first connect" do
      conn = Connection.new(__MODULE__, fn -> :unused end)

      assert conn.status == :disconnected
      refute conn.socket
      assert_received :connect
    end
  end

  describe "connect/1" do
    test "opens the socket", %{port: port} do
      conn = connected(port)

      assert Connection.connected?(conn)
      assert conn.attempt == 0
      refute conn.ping_timer
    end

    test "refused connect logs, schedules a retry, and does not raise" do
      conn = Connection.new(__MODULE__, Server.connect_fun(closed_port()))
      assert_received :connect

      {conn, log} = with_log(fn -> Connection.connect(conn) end)

      assert conn.status == :disconnected
      assert conn.attempt == 1
      assert log =~ "(Pigeon.HTTP.ConnectionTest) failed to connect"
      assert log =~ "connection refused"
      assert_receive :connect, 1_000
    end

    test "backoff grows with each failed attempt" do
      conn = Connection.new(__MODULE__, Server.connect_fun(closed_port()))
      assert_received :connect

      {conn, _log} = with_log(fn -> Connection.connect(conn) end)
      {conn, _log} = with_log(fn -> Connection.connect(conn) end)

      assert conn.attempt == 2
    end
  end

  describe "request/6" do
    test "fails the notification with not_connected while disconnected" do
      conn = Connection.new(__MODULE__, fn -> :unused end)

      ^conn = Connection.request(conn, "GET", "/ok", [], "", notification(:a))

      assert_receive {:response, %{tag: :a, response: :not_connected}}
    end

    test "delivers completed responses to the handler", %{port: port} do
      conn = connected(port)

      conn =
        Connection.request(conn, "GET", "/ok", [], "", notification(:a))

      pump_until(conn, fn conn ->
        Connection.connected?(conn) and queue_empty?(conn)
      end)

      assert_received {:done,
                       %Request{
                         status: 200,
                         body: "ok",
                         notification: %{tag: :a}
                       }}
    end
  end

  describe "request_sync/6" do
    test "returns the completed request", %{port: port} do
      conn = connected(port)

      {:ok, conn, %Request{status: 200, body: "ok"}} =
        Connection.request_sync(conn, "GET", "/ok", [], "")

      assert queue_empty?(conn)
    end

    test "times out and forgets the request", %{port: port} do
      conn = connected(port)

      {:error, conn, :timeout} =
        Connection.request_sync(
          conn,
          "GET",
          "/hold",
          [Server.reply_to(self())],
          "",
          100
        )

      assert Connection.connected?(conn)
      assert queue_empty?(conn)

      assert_receive {:held, plug}
      send(plug, :release)
    end
  end

  describe "disconnect" do
    test "fails in-flight requests with :timeout, then reconnects", %{
      server: server,
      port: port
    } do
      conn = connected(port)
      headers = [Server.reply_to(self())]

      conn =
        Connection.request(
          conn,
          "GET",
          "/hold",
          headers,
          "",
          notification(:held)
        )

      assert_receive {:held, _plug}

      {conn, log} =
        with_log(fn ->
          Server.stop(server)
          pump_until(conn, fn conn -> conn.status == :disconnected end)
        end)

      assert log =~ "disconnected"
      assert_receive {:response, %{tag: :held, response: :timeout}}
      assert_received :connect
      refute conn.socket
      assert queue_empty?(conn)

      {:ok, _server, ^port} = Server.start(port)
      {conn, log} = with_log(fn -> Connection.connect(conn) end)

      assert Connection.connected?(conn)
      assert log == ""
    end
  end

  describe "GOAWAY" do
    test "drains in-flight requests before reconnecting", %{port: port} do
      conn = connected(port)
      headers = [Server.reply_to(self())]

      conn =
        Connection.request(
          conn,
          "GET",
          "/hold",
          headers,
          "",
          notification(:held)
        )

      assert_receive {:held, plug}

      conn = inject(conn, goaway(last_stream_id: 0x7FFFFFFF))
      assert conn.status == :draining

      ^conn =
        Connection.request(conn, "GET", "/ok", [], "", notification(:refused))

      assert_receive {:response, %{tag: :refused, response: :not_connected}}

      send(plug, :release)

      {_conn, log} =
        with_log(fn ->
          pump_until(conn, fn conn -> conn.status == :disconnected end)
        end)

      assert_received {:done,
                       %Request{
                         status: 200,
                         body: "released",
                         notification: %{tag: :held}
                       }}

      assert log == ""
      assert_received :connect
    end

    test "fails unprocessed requests with :not_connected", %{port: port} do
      conn = connected(port)
      headers = [Server.reply_to(self())]

      conn =
        Connection.request(
          conn,
          "GET",
          "/hold",
          headers,
          "",
          notification(:held)
        )

      assert_receive {:held, plug}

      {conn, log} = with_log(fn -> inject(conn, goaway(last_stream_id: 0)) end)

      assert_receive {:response, %{tag: :held, response: :not_connected}}
      assert conn.status == :disconnected
      assert log == ""
      assert_received :connect

      send(plug, :release)
    end

    test "logs a GOAWAY that carries an error code", %{port: port} do
      conn = connected(port)

      {conn, log} =
        with_log(fn ->
          inject(conn, goaway(last_stream_id: 0, error_code: 0xB))
        end)

      assert conn.status == :disconnected
      assert log =~ "[error]"
      assert log =~ "enhance_your_calm"
      assert_received :connect
    end
  end

  describe "ping/1" do
    test "sends a ping and schedules the next one", %{port: port} do
      conn =
        __MODULE__
        |> Connection.new(Server.connect_fun(port), 20)
        |> Connection.connect()

      assert_receive :ping, 500

      conn = Connection.ping(conn)
      assert conn.ping_timer

      conn = pump(conn)
      assert Connection.connected?(conn)
      assert_receive :ping, 500
    end
  end

  describe "handle_message/3" do
    test "returns :unknown for unrelated messages", %{port: port} do
      conn = connected(port)

      assert Connection.handle_message(conn, :something_else, &noop/1) ==
               :unknown
    end
  end

  # Helpers

  defp closed_port, do: Server.closed_port()

  # Connects and completes one request. Mint stays in its handshake state until
  # it reads the server SETTINGS frame, and a GOAWAY during handshake is a hard
  # error, so injected frames need an established connection.
  defp connected(port) do
    conn = Connection.new(__MODULE__, Server.connect_fun(port))
    assert_received :connect
    {conn, _log} = with_log(fn -> Connection.connect(conn) end)

    {:ok, conn, %Request{status: 200}} =
      Connection.request_sync(conn, "GET", "/ok", [], "")

    conn
  end

  defp notification(tag) do
    pid = self()

    %{
      __meta__: %Pigeon.Metadata{
        on_response: fn n -> send(pid, {:response, n}) end
      },
      response: nil,
      tag: tag
    }
  end

  defp handler(request), do: send(self(), {:done, request})

  defp noop(_request), do: :ok

  defp queue_empty?(conn), do: Pigeon.HTTP.RequestQueue.empty?(conn.queue)

  # Feeds one socket message to the connection. Test protocol messages such as
  # `{:held, pid}` stay in the mailbox.
  defp pump(conn) do
    receive do
      message when is_tuple(message) and elem(message, 0) in @socket_tags ->
        case Connection.handle_message(conn, message, &handler/1) do
          {:ok, conn} -> conn
          :unknown -> conn
        end
    after
      2_000 -> flunk("no socket message arrived")
    end
  end

  defp pump_until(conn, done?) do
    if done?.(conn) do
      conn
    else
      conn |> pump() |> pump_until(done?)
    end
  end

  # Pretends the server sent a frame by handing it to the connection as if it
  # came off the socket.
  defp inject(conn, frame) do
    message = {:ssl, Mint.HTTP.get_socket(conn.socket), frame}
    {:ok, conn} = Connection.handle_message(conn, message, &handler/1)
    conn
  end

  # RFC 9113 section 6.8. Stream 0, no debug data. Defaults to NO_ERROR.
  defp goaway(opts) do
    last_stream_id = Keyword.fetch!(opts, :last_stream_id)
    error_code = Keyword.get(opts, :error_code, 0)

    <<8::24, 0x7::8, 0::8, 0::1, 0::31, 0::1, last_stream_id::31,
      error_code::32>>
  end
end
