defmodule Pigeon.ADM do
  @moduledoc """
  Amazon Device Messaging (ADM)
  """

  import Supervisor.Spec

  alias Pigeon.ADM.{Config, Notification, Worker}

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

  - `:to` - Defines worker to process push. Defaults to `:adm_default`
  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  """
  @type push_opts :: [
          to: atom | pid | nil,
          on_response: on_response | nil
        ]

  @type connection_response ::
          {:ok, pid}
          | {:error, {:already_started, pid}}

  @default_timeout 5_000

  @doc """
  Sends a push over ADM.

  ## Examples

      iex> msg = %{"body" => "your message"}
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> Pigeon.ADM.push(n, on_response: nil)
      :ok

      iex> msg = %{"body" => "your message"}
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> Pigeon.ADM.push(n)
      %Pigeon.ADM.Notification{consolidation_key: nil,
       expires_after: 604800, md5: "M13RuG4uDWqajseQcCiyiw==",
       payload: %{"data" => %{"body" => "your message"}},
       registration_id: "your_reg_id", response: :invalid_registration_id,
       updated_registration_id: nil}

      iex> msg = %{"body" => "your message"}
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> notifs = Pigeon.ADM.push([n, n], to: :adm_default)
      iex> Enum.map(notifs, & &1.response)
      [:invalid_registration_id, :invalid_registration_id]

      iex> me = self()
      iex> handler = fn(_x) -> send(me, "Sent a push!") end
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", %{})
      iex> Pigeon.ADM.push(n, on_response: handler)
      iex> receive do
      ...>   x -> x
      ...> after
      ...>   5_000 -> "No push response..."
      ...> end
      "Sent a push!"

      iex> msg = %{"body" => "your message"}
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> notif = Pigeon.ADM.push(n, to: :worker_not_started)
      iex> notif.response
      :timeout
  """
  @spec push(Notification.t() | [Notification.t()], Keyword.t()) :: no_return
  def push(notifications, opts \\ [])

  def push(notifications, opts) when is_list(notifications) do
    worker_name = opts[:to] || Config.default_name()

    if Keyword.has_key?(opts, :on_response) do
      cast_push(worker_name, notifications, opts[:on_response])
    else
      notifications
      |> Enum.map(&Task.async(fn -> sync_push(worker_name, &1) end))
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

  def push(notification, opts) do
    worker_name = opts[:to] || Config.default_name()

    if Keyword.has_key?(opts, :on_response) do
      cast_push(worker_name, notification, opts[:on_response])
    else
      sync_push(worker_name, notification)
    end
  end

  defp cast_push(worker_name, notifications, on_response)
       when is_list(notifications) do
    for n <- notifications, do: cast_push(worker_name, n, on_response)
  end

  defp cast_push(worker_name, notification, on_response) do
    GenServer.cast(worker_name, {:push, :adm, notification, on_response})
  end

  defp sync_push(worker_name, notification) do
    pid = self()
    ref = :erlang.make_ref()
    on_response = fn x -> send(pid, {ref, x}) end

    GenServer.cast(worker_name, {:push, :adm, notification, on_response})

    receive do
      {^ref, x} -> x
    after
      @default_timeout -> %{notification | response: :timeout}
    end
  end

  @doc ~S"""
  Starts ADM worker connection with given config or name.

  ## Examples

      iex> config = Pigeon.ADM.Config.new(:adm_default)
      iex> {:ok, pid} = Pigeon.ADM.start_connection(%{config | name: nil})
      iex> Process.alive?(pid)
      true
  """
  @spec start_connection(atom | Config.t() | Keyword.t()) :: connection_response
  def start_connection(name) when is_atom(name) do
    config = Config.new(name)
    Supervisor.start_child(:pigeon, worker(Worker, [config], id: name))
  end

  def start_connection(%Config{} = config) do
    Worker.start_link(config)
  end

  def start_connection(opts) when is_list(opts) do
    opts
    |> Config.new()
    |> start_connection()
  end

  @doc ~S"""
  Stops existing ADM worker connection.

  ## Examples

      iex> config = Pigeon.ADM.Config.new(:adm_default)
      iex> {:ok, pid} = Pigeon.ADM.start_connection(%{config | name: nil})
      iex> Pigeon.ADM.stop_connection(pid)
      :ok
      iex> :timer.sleep(500)
      iex> Process.alive?(pid)
      false
  """
  @spec stop_connection(atom | pid) :: :ok
  def stop_connection(name), do: Worker.stop_connection(name)
end
