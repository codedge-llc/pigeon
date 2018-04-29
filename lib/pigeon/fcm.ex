defmodule Pigeon.FCM do
  @moduledoc """
  Firebase Cloud Messaging (FCM)
  """

  require Logger
  import Supervisor.Spec

  alias Pigeon.FCM.{Config, Notification}
  alias Pigeon.Worker

  @typedoc ~S"""
  Can be either a single notification or a list.
  """
  @type notification :: Notification.t() | [Notification.t(), ...]

  @typedoc ~S"""
  Async callback for push notification response.

  ## Examples

    handler = fn(n) ->
      case n.status do
        :success ->
          bad_regids = FCM.Notification.remove?(n)
          to_retry = FCM.Notification.retry?(n)
          # Handle updated regids, remove bad ones, etc
        :unauthorized ->
          # Bad FCM key
        error ->
          # Some other error
      end
    end

    n = Pigeon.FCM.Notification.new("device token", %{}, %{"message" => "test"})
    Pigeon.FCM.push(n, on_response: handler)
  """
  @type on_response :: (Notification.t() -> no_return)

  @typedoc ~S"""
  Options for sending push notifications.

  - `:to` - Defines worker to process push. Defaults to `:fcm_default`
  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  - `:timeout` - Specifies timeout for push responses. Useful if sending large
    batches synchronously.
  """
  @type push_opts :: [
          to: atom | pid | nil,
          timeout: pos_integer | nil,
          on_response: on_response | nil
        ]

  @default_timeout 5_000
  @default_worker :fcm_default

  @doc ~S"""
  Sends a push over FCM.

  ## Examples

      iex> n = Pigeon.FCM.Notification.new("regId", %{}, %{"message" => "123"})
      iex> Pigeon.FCM.push(n)
      %Pigeon.FCM.Notification{message_id: nil,
       payload: %{"data" => %{"message" => "123"}}, priority: :normal,
       registration_id: "regId", status: :success, response:
       [invalid_registration: "regId"]}

      iex> n = Pigeon.FCM.Notification.new("regId", %{}, %{"message" => "123"})
      iex> Pigeon.FCM.push(n, on_response: nil)
      :ok

      iex> n = Pigeon.FCM.Notification.new(["regId", "regId"], %{},
      ...> %{"message" => "123"})
      iex> Pigeon.FCM.push(n)
      %Pigeon.FCM.Notification{message_id: nil,
       payload: %{"data" => %{"message" => "123"}}, priority: :normal,
       registration_id: ["regId", "regId"], status: :success,
       response: [invalid_registration: "regId",
       invalid_registration: "regId"]}

      iex> n = Pigeon.FCM.Notification.new(["regId", "regId"], %{},
      ...> %{"message" => "test"})
      iex> notifs = Pigeon.FCM.push([n, n])
      iex> Enum.map(notifs, & &1.response)
      [[invalid_registration: "regId", invalid_registration: "regId"],
       [invalid_registration: "regId", invalid_registration: "regId"]]
  """
  @spec push(notification, Keyword.t()) :: notification | :ok
  def push(notification, opts \\ [])

  def push(notification, opts) when is_list(notification) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    if Keyword.has_key?(opts, :on_response) do
      for n <- notification, do: send_push(n, opts[:on_response], opts)
      :ok
    else
      notification
      |> Enum.map(&Task.async(fn -> sync_push(&1, opts) end))
      |> Task.yield_many(timeout)
      |> Enum.map(&task_mapper(&1))
    end
  end

  def push(notification, opts) do
    if Keyword.has_key?(opts, :on_response) do
      send_push(notification, opts[:on_response], opts)
      :ok
    else
      sync_push(notification, opts)
    end
  end

  defp task_mapper({task, result}) do
    case result do
      nil -> Task.shutdown(task, :brutal_kill)
      {:ok, notif} -> notif
    end
  end

  defp send_push(notifications, on_response, opts)
       when is_list(notifications) do
    worker_name = opts[:to] || @default_worker

    notifications
    |> Enum.map(&cast_request(worker_name, &1, on_response, opts))
  end

  defp send_push(notification, on_response, opts) do
    send_push([notification], on_response, opts)
  end

  defp cast_request(worker_name, request, on_response, opts) do
    opts = Keyword.put(opts, :on_response, on_response)
    Worker.send_push(worker_name, request, opts)
  end

  defp sync_push(notification, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    ref = :erlang.make_ref()
    pid = self()
    on_response = fn x -> send(pid, {ref, x}) end
    send_push(notification, on_response, opts)

    receive do
      {^ref, x} -> x
    after
      timeout -> %{notification | status: :timeout}
    end
  end

  @doc ~S"""
  Starts FCM worker connection with given config or name.

  ## Examples

      iex> config = Pigeon.FCM.Config.new(:fcm_default)
      iex> {:ok, pid} = Pigeon.FCM.start_connection(%{config | name: nil})
      iex> Process.alive?(pid)
      true
  """
  def start_connection(opts \\ [])

  def start_connection(name) when is_atom(name) do
    worker = worker(Pigeon.Worker, [Config.new(name)], id: name)
    Supervisor.start_child(:pigeon, worker)
  end

  def start_connection(%Config{} = config) do
    Worker.start_link(config)
  end

  def start_connection(opts) do
    opts
    |> Config.new()
    |> start_connection()
  end

  @doc ~S"""
  Stops existing FCM worker connection.

  ## Examples

      iex> config = Pigeon.FCM.Config.new(:fcm_default)
      iex> {:ok, pid} = Pigeon.FCM.start_connection(%{config | name: nil})
      iex> Pigeon.FCM.stop_connection(pid)
      :ok
      iex> :timer.sleep(500)
      iex> Process.alive?(pid)
      false
  """
  @spec stop_connection(atom | pid) :: :ok
  def stop_connection(name), do: Worker.stop_connection(name)
end
