defmodule Pigeon.DispatcherWorker do
  @moduledoc false

  use GenServer

  defmodule TimingData do
    @doc """
    Maintain an exponential moving average of response times
    """
    @default_alpha 0.2
    @default_initial_average_ms 100
    defstruct [:average_native, :alpha]

    @type t :: %__MODULE__{
            average_native: non_neg_integer,
            alpha: float
          }

    def new(opts \\ []) do
      average_ms =
        Keyword.get(opts, :initial_average_ms, @default_initial_average_ms)

      average_native =
        System.convert_time_unit(average_ms, :millisecond, :native)

      alpha = Keyword.get(opts, :alpha, @default_alpha)
      %TimingData{average_native: average_native, alpha: alpha}
    end

    @spec update(t(), non_neg_integer()) :: %TimingData{}
    def update(d = %{average_native: avg, alpha: a}, t) do
      %{d | average_native: round(a * t + (1 - a) * avg)}
    end
  end

  def start_link(opts) do
    opts[:adapter] || raise "adapter is not specified"
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    case opts[:adapter].init(opts) do
      {:ok, state} ->
        Pigeon.Registry.register(opts[:supervisor], 0)

        state =
          state
          |> Map.put(:timing_data, TimingData.new(opts))
          |> Map.put(:on_timeout, opts[:on_timeout])
          |> Map.put(:supervisor, opts[:supervisor])
          |> Map.put(:adapter, opts[:adapter])
          |> Map.put(:penalty_box, false)

        {:ok, state}

      {:error, reason} ->
        {:error, reason}

      {:stop, reason} ->
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_info({:"$push", notification}, state) do
    case state.adapter.handle_push(notification, state) do
      {:noreply, new_state} -> {:noreply, new_state}
      {:stop, reason, new_state} -> {:stop, reason, new_state}
    end
  end

  def handle_info({:set_penalty_box, value, duration_ms}, state) do
    {:noreply, set_penalty_box(state, value, duration_ms)}
  end

  def handle_info(msg, state) do
    case state.adapter.handle_info(msg, state) do
      {:noreply, new_state} -> {:noreply, new_state}
      {:stop, reason, new_state} -> {:stop, reason, new_state}
    end
  end

  @impl GenServer
  def handle_call(:info, _from, state) do
    average_response_time_ms =
      System.convert_time_unit(
        state.timing_data.average_native,
        :native,
        :millisecond
      )

    info = %{average_response_time_ms: average_response_time_ms}

    {:reply, info, state}
  end

  @impl GenServer
  def handle_cast({:update_timing_data, response_time_native}, state) do
    state =
      Map.update!(
        state,
        :timing_data,
        &TimingData.update(&1, response_time_native)
      )

    update_priority(state)
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast(:handle_timeout, state) do
    case state[:on_timeout] do
      :stop ->
        {:stop, :timeout, state}

      {:penalty_box, duration_ms} ->
        {:noreply, set_penalty_box(state, true, duration_ms)}

      _ ->
        {:noreply, state}
    end
  end

  defp set_penalty_box(state, true, duration_ms) do
    Process.send_after(self(), {:set_penalty_box, false, nil}, duration_ms)
    state = %{state | penalty_box: true}
    update_priority(state)
  end

  defp set_penalty_box(state, false, _) do
    state = %{state | penalty_box: false}
    update_priority(state)
  end

  defp update_priority(state = %{penalty_box: true}) do
    Pigeon.Registry.unregister(state.supervisor)
    Pigeon.Registry.register(state.supervisor, :infinity)
    state
  end

  defp update_priority(state) do
    mailbox_size = Process.info(self())[:message_queue_len]
    p = state.timing_data.average_native * mailbox_size
    Pigeon.Registry.unregister(state.supervisor)
    Pigeon.Registry.register(state.supervisor, p)
    state
  end
end
