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
        Pigeon.Registry.register(opts[:supervisor])

        state =
          state
          |> Map.put(:timing_data, TimingData.new(opts))
          |> Map.put(:on_timeout, opts[:on_timeout])

        {:ok, %{adapter: opts[:adapter], state: state}}

      {:error, reason} ->
        {:error, reason}

      {:stop, reason} ->
        {:stop, reason}
    end
  end

  @impl GenServer
  def handle_info({:"$push", notification}, %{adapter: adapter, state: state}) do
    case adapter.handle_push(notification, state) do
      {:noreply, new_state} ->
        {:noreply, %{adapter: adapter, state: new_state}}

      {:stop, reason, new_state} ->
        {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end

  def handle_info(msg, %{adapter: adapter, state: state}) do
    case adapter.handle_info(msg, state) do
      {:noreply, new_state} ->
        {:noreply, %{adapter: adapter, state: new_state}}

      {:stop, reason, new_state} ->
        {:stop, reason, %{adapter: adapter, state: new_state}}
    end
  end

  @impl GenServer
  def handle_call(:info, _from, %{adapter: adapter, state: state}) do
    average_response_time_ms =
      System.convert_time_unit(
        state.timing_data.average_native,
        :native,
        :millisecond
      )

    info = %{
      peername: peername(state),
      average_response_time_ms: average_response_time_ms
    }

    {:reply, info, %{adapter: adapter, state: state}}
  end

  @impl GenServer
  def handle_cast({:update_timing_data, response_time_native}, %{
        adapter: adapter,
        state: state
      }) do
    state =
      Map.update!(
        state,
        :timing_data,
        &TimingData.update(&1, response_time_native)
      )

    {:noreply, %{adapter: adapter, state: state}}
  end

  @impl GenServer
  def handle_cast(:handle_timeout, s) do
    if s.state[:on_timeout] == :stop do
      {:stop, :timeout, s}
    else
      {:noreply, s}
    end
  end

  defp peername(state) do
    with %{socket: socket} <- state,
         %{connection: connection} <- :sys.get_state(socket),
         %{config: %{socket: socket2}} <- :sys.get_state(connection),
         %{socket: socket3} <- :sys.get_state(socket2),
         {_, {_, port, _, _}, _} <- socket3,
         {:ok, addr} <- :inet.peername(port) do
      addr
    else
      _ -> "unknown"
    end
  end
end
