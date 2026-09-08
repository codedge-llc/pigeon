defmodule PigeonTest.Server do
  @moduledoc false

  # Local HTTP/2 TLS server for connection tests.
  #
  # Routes:
  #
  # - `GET /ok` responds 200 at once.
  # - `GET /hold` tells the process named in the `x-reply-to` header that
  #   the request arrived, then waits for `:release` before it responds.

  @behaviour Plug

  import Plug.Conn

  @cert Path.expand("FakeServerCert.pem", __DIR__)
  @key Path.expand("FakeServerKey.pem", __DIR__)

  # Port 0 asks the kernel for a free port.
  @spec start(:inet.port_number()) :: {:ok, pid(), :inet.port_number()}
  def start(port \\ 0) do
    {:ok, pid} =
      Bandit.start_link(
        plug: __MODULE__,
        scheme: :https,
        ip: :loopback,
        port: port,
        certfile: @cert,
        keyfile: @key,
        startup_log: false
      )

    {:ok, {_ip, port}} = ThousandIsland.listener_info(pid)
    {:ok, pid, port}
  end

  # Bandit sends GOAWAY on every open connection and closes it.
  @spec stop(pid()) :: :ok
  def stop(pid), do: ThousandIsland.stop(pid, 1_000)

  # A port nothing listens on. Grabs a free one from the kernel, then releases it.
  @spec closed_port() :: :inet.port_number()
  def closed_port do
    {:ok, socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  @spec connect_fun(:inet.port_number()) :: Pigeon.HTTP.Connection.connect_fun()
  def connect_fun(port) do
    fn ->
      Mint.HTTP.connect(:https, "localhost", port,
        protocols: [:http2],
        transport_opts: [verify: :verify_none]
      )
    end
  end

  # Header that tells `/hold` where to report the held request.
  @spec reply_to(pid()) :: {String.t(), String.t()}
  def reply_to(pid),
    do: {"x-reply-to", pid |> :erlang.pid_to_list() |> to_string()}

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(%{request_path: "/ok"} = conn, _opts) do
    send_resp(conn, 200, "ok")
  end

  def call(%{request_path: "/hold"} = conn, _opts) do
    [reply_to] = get_req_header(conn, "x-reply-to")
    send(reply_to |> to_charlist() |> :erlang.list_to_pid(), {:held, self()})

    receive do
      :release -> send_resp(conn, 200, "released")
    after
      5_000 -> send_resp(conn, 504, "never released")
    end
  end
end
