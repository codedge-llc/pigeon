defmodule Pigeon.APNSWorker do  
  use GenServer
  require Logger

  def start_link(name, mode, cert, key) do
    Logger.debug("Starting worker #{name}\n\t mode: #{mode}, cert: #{cert}, key: #{key}")
    GenServer.start_link(__MODULE__, {:ok, mode, cert, key}, name: name)
  end

  def stop() do
    :gen_server.cast(self, :stop)
  end

  def init({:ok, mode, cert, key} = args) do
    {:ok, socket} = Pigeon.HTTP2.connect(mode, cert, key)
    state = %{
      apns_socket: socket,
      mode: mode,
      cert: cert,
      key: key
    }
    {:ok, data} = Pigeon.HTTP2.send_connection_preface(socket)
    response = Pigeon.HTTP2.establish_connection(socket)
    {:ok, state}
  end

  def handle_call({:start_connection, mode, cert, key}, from, state) do
    c = Pigeon.APNS.Connection.new(mode, cert, key)
    state = %{
      apns_socket: c,
      mode: mode,
      cert: cert,
      key: key
    }
    {:noreply, state}
  end

  def handle_call({:push, :apns, notification}, from, state) do 
    %{apns_socket: c, mode: mode, cert: cert, key: key} = state
    %{device_token: device_token, topic: topic, payload: payload} = notification

    push_header = Pigeon.HTTP2.push_header_frame(mode, device_token, topic, payload)
    push_data = Pigeon.HTTP2.push_data_frame(payload)

    :ssl.send(c, push_header)
    :ssl.send(c, push_data)

    { :reply, :ok, state }
  end

  def handle_info({:ssl, socket, bin}, %{apns_socket: socket} = state) do
    Logger.debug "Got APNS response data..."
    { :noreply, state }
  end

  def handle_info({:ssl_closed, socket}, %{apns_socket: socket, mode: mode, cert: cert, key: key} = state) do
    Logger.debug("Got connection close...")
    c = Pigeon.APNS.Connection.new(mode, cert, key)

    {:noreply, %{state | apns_socket: c}}
  end

  @doc "Handle the server stop message"
  def handle_cast(:stop , state) do
    { :noreply, state }
  end
end
