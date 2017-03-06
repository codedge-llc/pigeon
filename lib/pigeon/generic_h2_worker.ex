defmodule Pigeon.GenericH2Worker do
  defmacro __using__(_opts) do
    quote do
      use GenServer
      require Logger

      def start_link(name, config) do
        start_link(Keyword.merge(config, [name: name]))
      end

      def start_link(config) do
        GenServer.start_link(__MODULE__, config, name: config[:name])
      end

      def stop, do: :gen_server.cast(self(), :stop)

      def init(config) do
        Process.flag(:trap_exit, true)
        {:ok, new_state(config)}
      end

      defp new_state(config, socket \\ nil) do
        %{
          socket: socket,
          queue: %{},
          config: config
        }
      end

      def initialize_worker(config) do
        mode = config[:mode]
        case connect_socket(config, 0) do
          {:ok, socket} ->
            {:ok, new_state(config, socket)}
          {:closed, _socket} ->
            Logger.error """
              Socket closed unexpectedly.
              """
            {:ok, new_state(config)}
          {:error, :timeout} ->
            Logger.error """
              Failed to establish SSL connection. Is the certificate signed for :#{mode} mode?
              """
            {:ok, new_state(config)}
          {:error, :invalid_config} ->
            Logger.error """
              Invalid configuration.
              """
            {:stop, {:error, :invalid_config}}
        end
      end

      def connect_socket(_config, 3), do: {:error, :timeout}
      def connect_socket(config, tries) do
        uri = host(config)
        case socket_options(config) do
          {:ok, options} -> do_connect_socket(config, uri, options, tries)
          error -> error
        end
      end

      defp do_connect_socket(config, uri, options, tries) do
        case Pigeon.H2.open(uri, port(config), options) do
          {:ok, socket} -> {:ok, socket}
          {:error, reason} ->
            Logger.error(inspect(reason))
            connect_socket(config, tries + 1)
        end
      end

      def handle_cast(:stop, state), do: { :noreply, state }

      def handle_cast({:push, _, notification}, state) do
        send_push(state, notification, nil)
      end

      def handle_cast({:push, _, notification, on_response}, state) do
        send_push(state, notification, on_response)
      end

      def handle_cast(_msg, state) do
        {:noreply, state}
      end

      def send_push(%{socket: nil, config: config}, notification, on_reponse) do
        Logger.info "Reconnecting to push service provider before request"
        case initialize_worker(config) do
          {:ok, newstate} -> send_push(newstate, notification, on_reponse)
          error -> error
        end
      end

      def send_push(state, notification, on_response) do
        %{socket: socket, queue: queue, config: config} = state
        json = Pigeon.Notification.json_payload(notification.payload)

        headers = req_headers(config, notification)
        uri = host(config)
        path = req_path(notification)
        case Pigeon.H2.post(socket, uri, path, headers, json) do
          {:ok, stream_id} ->
            new_q = Map.put(queue, "#{stream_id}", {notification, on_response})
            {:noreply, %{state | queue: new_q }}
          {:error, reason} ->
            maybe_respond({:error, reason}, on_response)
            {:noreply, state}
        end
      end

      defp maybe_respond(response, on_response) do
        unless on_response == nil, do: on_response.(response)
      end

      def handle_info({:END_STREAM, stream_id}, %{socket: socket, queue: queue} = state) do
        {:ok, {headers, body}} = Pigeon.H2.receive(socket, stream_id)

        {notification, on_response} = queue["#{stream_id}"]
        case get_status(headers) do
          "200" ->
            notification = parse_response(notification, headers, body)
            maybe_respond({:ok, notification}, on_response)
            new_queue = Map.delete(queue, "#{stream_id}")
          nil ->
            :ok
          code ->
            reason =
              try do
                parse_error(notification, headers, body)
              catch
                _, unknown -> unknown
              end
            log_error(code, reason, notification)
            maybe_respond({:error, reason, notification}, on_response)
        end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      end

      def handle_info({:ping, _from}, state), do: {:noreply, state}

      def handle_info({:ok, _from}, state), do: {:noreply, state}

      def handle_info({:EXIT, socket, _}, %{socket: socket} = state) do
        {:noreply, %{state | socket: nil}}
      end
      def handle_info({:EXIT, _socket, _}, state) do
        {:noreply, state}
      end

      def handle_info(unknown, state) do
        Logger.warn(~s"Unknown info #{inspect unknown}")
        {:noreply, state}
      end

      defp get_status(nil), do: nil
      defp get_status(headers) do
        case Enum.find(headers, fn({key, _val}) -> key == ":status" end) do
          {":status", status} -> status
          nil -> nil
        end
      end

      defp log_error(code, reason, notification) do
        Logger.error("Error code #{{code}} (#{reason}): #{error_msg(code, reason)}")
      end

    end
  end
end
