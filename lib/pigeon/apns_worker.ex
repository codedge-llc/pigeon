defmodule Pigeon.APNSWorker do  
  use GenServer
  require Logger

  @doc "Starts the worker"
  def start_link(name, mode, cert, key) do
    Logger.debug("Starting worker #{name}\n\t mode: #{mode}, cert: #{cert}, key: #{key}")
    GenServer.start_link(__MODULE__, {:ok, mode, cert, key}, name: name)
  end

  @doc "Stops the server"
  def stop() do
    :gen_server.cast(self, :stop)
  end

  @doc "Initialize our server"
  def init(args) do
   {:ok, mode, cert, key} = args
    Logger.debug("Doing the init...")
    #c = Pigeon.APNS.Connection.new(mode, cert, key)
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

  @doc "Implement this multiple times with a different pattern to deal
  with sync messages"
  def handle_call({:push, :apns, notification}, from, state) do 
    %{apns_socket: c, mode: mode, cert: cert, key: key} = state
    %{device_token: device_token, topic: topic, payload: payload} = notification
    push_header = Pigeon.HTTP2.push_header_frame(mode, device_token, topic, payload)
    push_data = Pigeon.HTTP2.push_data_frame(payload)

    :ssl.send(c, push_header)
    :ssl.send(c, push_data)

    #case :ssl.send(c, notification) do
    #  :ok -> 
    #    Logger.debug "Sent ok..."
		#		# receive do
		#		# 	{:ssl, socket, data} ->
    #    #     {status, identifier} = Pigeon.APNS.Connection.parse_response(data)
    #    #     IO.inspect Pigeon.APNS.Connection.parse_status(status)
    #    #     IO.inspect identifier

    #    #     c = Pigeon.APNS.Connection.new(mode, cert, key)
    #    #     state = %{
    #    #       apns_socket: c,
    #    #       mode: mode,
    #    #       cert: cert,
    #    #       key: key
    #    #     }
    #    #     { :reply, :ok, state }
		#		# 	after 0 ->
    #    #     Logger.debug("Timeout...")
		#		# end
    #  error -> Logger.error(error)
    #end
    { :reply, :ok, state }
  end

  def handle_info({:ssl, socket, bin}, %{apns_socket: socket} = state) do
    Logger.debug "Got data!"
    { :reply, :ok, state }
  end

  def handle_info({:ssl_closed, socket}, %{apns_socket: socket, mode: mode, cert: cert, key: key} = state) do
    Logger.debug("Got connection close...")
    c = Pigeon.APNS.Connection.new(mode, cert, key)

    {:noreply, %{state | apns_socket: c}}
  end

  def handle_call(message, from, state) do
    Logger.debug "Bad message..."
    {:reply, {:error, :bad_message}, state}
  end

  @doc "Implement this multiple times with a different pattern to deal
  with async messages"
  def handle_cast(:message, state) do
  end

  @doc "Handle the server stop message"
  def handle_cast(:stop , state) do
    { :noreply, state }
  end

  @doc "Implement this to handle out of band messages (messages not
  sent using a gen_server call)"
  def handle_info(message, state) do
    #{:reply, {:error, :bad_message}, state}
    { :noreply, state }
  end
end
