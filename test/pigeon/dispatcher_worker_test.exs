defmodule Pigeon.DispatcherWorkerTest do
  use ExUnit.Case

  alias Pigeon.DispatcherWorker

  defmodule ProbeAdapter do
    @default_state %{last_message: nil, all_messages: [], response_queue: []}

    def init(_opts), do: {:ok, @default_state}

    def handle_push(notification, state) do
      [response | rest_responses] = get_response_queue(state)

      state =
        %{
          state
          | last_message: notification,
            all_messages: state.all_messages ++ [notification],
            response_queue: rest_responses
        }

      case response do
        :noreply -> {:noreply, state}
        {:stop, reason} -> {:stop, reason, state}
      end
    end

    def handle_info({:probe, f}, state),
      do: {:noreply, apply(__MODULE__, f, [state])}

    def handle_info({:probe, f, a}, state),
      do: {:noreply, apply(__MODULE__, f, a ++ [state])}

    def set_next_response(response, state) do
      %{state | response_queue: [response]}
    end

    def set_response_queue(responses, state) do
      %{state | response_queue: responses}
    end

    def last_message(to, state), do: send(to, state.last_message)
    def all_messages(to, state), do: send(to, state.all_messages)
    def clear(state), do: Map.merge(state, @default_state)

    defp get_response_queue(%{response_queue: []}), do: [:noreply]
    defp get_response_queue(%{response_queue: queue}), do: queue
  end

  describe "startup" do
    test "Throws if adapter not specified" do
      assert_raise RuntimeError, fn ->
        DispatcherWorker.start_link([])
      end
    end

    test "Starts the GenServer" do
      {:ok, pid} = DispatcherWorker.start_link(adapter: ProbeAdapter)
      assert is_pid(pid)
    end

    test "Registers itself" do
      {:ok, pid} =
        DispatcherWorker.start_link(adapter: ProbeAdapter, supervisor: self())

      assert Registry.lookup(Pigeon.Registry, self()) == [{pid, 0}]
    end
  end

  describe "push" do
    setup [:create_worker, :clear_probe]

    test "Sends push to the adapter", ctx do
      send(ctx.worker, {:"$push", "msg"})
      send(ctx.worker, {:probe, :last_message, [self()]})
      assert_receive "msg"
    end

    test "stops if the adapter tells it to", ctx do
      Process.flag(:trap_exit, true)
      send(ctx.worker, {:probe, :set_next_response, [{:stop, :oh_noes}]})
      send(ctx.worker, {:"$push", "msg"})
      assert_receive {:EXIT, _, :oh_noes}
      Process.flag(:trap_exit, false)
    end
  end

  describe "timing data" do
    setup [:create_worker]

    test "retries default timing data", ctx do
      info = GenServer.call(ctx.worker, :info)
      assert %{average_response_time_ms: 100} = info
    end

    test "updates timing data", ctx do
      t = System.convert_time_unit(200, :millisecond, :native)
      GenServer.cast(ctx.worker, {:update_timing_data, t})
      info = GenServer.call(ctx.worker, :info)
      assert %{average_response_time_ms: 120} = info
    end

    test "allows you to configure the initial time and alpha values", _ctx do
      {:ok, worker} =
        DispatcherWorker.start_link(
          adapter: ProbeAdapter,
          initial_average_ms: 500,
          alpha: 0.5
        )

      info = GenServer.call(worker, :info)
      assert %{average_response_time_ms: 500} = info

      t = System.convert_time_unit(1000, :millisecond, :native)
      GenServer.cast(worker, {:update_timing_data, t})
      info = GenServer.call(worker, :info)
      assert %{average_response_time_ms: 750} = info
    end

    test "updates the priority with the registry", ctx do
      t = System.convert_time_unit(200, :millisecond, :native)
      GenServer.cast(ctx.worker, {:update_timing_data, t})
      _ = GenServer.call(ctx.worker, :info)
      p = System.convert_time_unit(120, :millisecond, :native)
      assert Registry.lookup(Pigeon.Registry, self()) == [{ctx.worker, p}]
    end
  end

  describe "timeouts" do
    test "does nothing by default", _ctx do
      {:ok, worker} =
        DispatcherWorker.start_link(adapter: ProbeAdapter, supervisor: self())

      GenServer.cast(worker, :handle_timeout)
      _ = GenServer.call(worker, :info)
      assert Registry.lookup(Pigeon.Registry, self()) == [{worker, 0}]
    end

    test "stops if requested", _ctx do
      {:ok, worker} =
        DispatcherWorker.start_link(
          adapter: ProbeAdapter,
          supervisor: self(),
          on_timeout: :stop
        )

      Process.flag(:trap_exit, true)
      GenServer.cast(worker, :handle_timeout)
      assert_receive {:EXIT, _, :timeout}
      Process.flag(:trap_exit, false)
    end

    test "Puts itself in the penalty box", _ctx do
      {:ok, worker} =
        DispatcherWorker.start_link(
          adapter: ProbeAdapter,
          supervisor: self(),
          on_timeout: {:penalty_box, 100}
        )

      GenServer.cast(worker, :handle_timeout)
      _ = GenServer.call(worker, :info)
      worker_list = Registry.lookup(Pigeon.Registry, self())
      assert worker_list == [{worker, :infinity}]
      Process.sleep(110)
      worker_list = Registry.lookup(Pigeon.Registry, self())
      assert worker_list == [{worker, 0}]
    end
  end

  describe "peername" do
    setup [:create_worker]

    test "returns unknown if no socket", ctx do
      info = GenServer.call(ctx.worker, :info)
      assert %{peername: "unknown"} = info
    end
  end

  defp create_worker(_ctx) do
    {:ok, pid} =
      DispatcherWorker.start_link(adapter: ProbeAdapter, supervisor: self())

    {:ok, worker: pid}
  end

  defp clear_probe(ctx) do
    send(ctx.worker, {:probe, :clear})
    :ok
  end
end
