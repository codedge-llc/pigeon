defmodule Pigeon.Dispatcher do
  defstruct adapter: nil, state: nil

  use GenServer

  @default_timeout 5_000

  @typedoc ~S"""
  Async callback for push notifications response.

  ## Examples

      handler = fn(%Pigeon.ADM.Notification{response: response}) ->
        case response do
          :success ->
            Logger.debug "Push successful!"
          :unregistered ->
            Logger.error "Bad device token!"
          _error ->
            Logger.error "Some other error happened."
        end
      end

      n = Pigeon.ADM.Notification.new("token", %{"message" => "test"})
      Pigeon.ADM.push(n, on_response: handler)
  """
  @type on_response :: (Notification.t() -> no_return)

  @typedoc ~S"""
  Options for sending push notifications.

  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  """
  @type push_opts :: [
          on_response: on_response | nil
        ]

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @adapter opts[:adapter]

      def child_spec(config) do
        opts = [[adapter: @adapter, config: config, name: __MODULE__]]

        %{
          id: __MODULE__,
          start: {Pigeon.Dispatcher, :start_link, opts},
          type: :worker
        }
      end

      def push(notification) do
        Pigeon.Dispatcher.push(__MODULE__, notification)
      end

      def push(notification, opts) do
        Pigeon.Dispatcher.push(__MODULE__, notification, opts)
      end
    end
  end

  def start_link(opts) do
    opts[:adapter] || raise "adapter is not specified"
    opts[:config] || raise "config is not specified"
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  @impl true
  def init(opts) do
    state = opts[:adapter].initial_state(opts[:config])

    {:ok, %__MODULE__{adapter: opts[:adapter], state: state}}
  end

  @spec push(pid | atom, notification :: struct | [struct], push_opts) ::
          {:ok, notification :: struct}
          | {:error, notification :: struct}
          | :ok
  def push(pid, notifications, opts \\ [])

  def push(pid, notifications, opts) when is_list(notifications) do
    if Keyword.has_key?(opts, :on_response) do
      push_async(pid, notifications, opts[:on_response])
    else
      notifications
      |> Enum.map(&Task.async(fn -> push_sync(pid, &1) end))
      |> Task.yield_many(@default_timeout + 500)
      |> Enum.map(fn {task, response} ->
        case response do
          nil -> Task.shutdown(task, :brutal_kill)
          {:ok, resp} -> resp
          _error -> nil
        end
      end)
    end
  end

  def push(pid, notification, opts) do
    if Keyword.has_key?(opts, :on_response) do
      push_async(pid, notification, opts[:on_response])
    else
      push_sync(pid, notification)
    end
  end

  defp push_async(pid, notifications, on_response)
       when is_list(notifications) do
    for n <- notifications, do: push_async(pid, n, on_response)
  end

  defp push_async(pid, notification, on_response) do
    GenServer.cast(pid, {:push, notification, on_response})
  end

  defp push_sync(pid, notification) do
    myself = self()
    ref = :erlang.make_ref()
    on_response = fn x -> send(myself, {ref, x}) end

    GenServer.cast(pid, {:push, notification, on_response})

    receive do
      {^ref, x} -> x
    after
      @default_timeout -> %{notification | response: :timeout}
    end
  end

  @impl true
  def handle_cast({:push, notification, on_response}, %{adapter: adapter, state: state}) do
    state = adapter.handle_push(notification, on_response, state)
    {:noreply, %__MODULE__{adapter: adapter, state: state}}
  end

  @impl true
  def handle_info(msg, %{adapter: adapter, state: state}) do
    {:noreply, state} = adapter.handle_info(msg, state)
    {:noreply, %__MODULE__{adapter: adapter, state: state}}
  end
end
