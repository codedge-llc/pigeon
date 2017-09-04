defmodule Pigeon.FCM do
  @moduledoc """
  Firebase Cloud Messaging (FCM)
  """

  require Logger
  import Supervisor.Spec

  alias Pigeon.FCM.{Config, Notification, NotificationResponse}

  @type on_response :: ((NotificationResponse.t) -> no_return)

  @typedoc ~S"""
  Options for sending push notifications.

  - `:to` - Defines worker to process push. Defaults to `:apns_default`
  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  """
  @type push_opts :: [
    to: atom | pid | nil,
    on_response: on_response | nil
  ]

  @default_timeout 5_000
  @default_worker :fcm_default
  @chunk_size 1_000

  @spec push(Notification.t, Keyword.t) :: {:ok, NotificationResponse.t}
  @spec push([Notification.t, ...], Keyword.t) :: [NotificationResponse.t, ...]
  def push(notification, opts \\ [])
  def push(notification, opts) when is_list(notification) do
    case opts[:on_response] do
      nil ->
        tasks = for n <- notification, do: Task.async(fn -> sync_push(n, opts) end)
        tasks
        |> Task.yield_many(@default_timeout + 10_000)
        |> Enum.map(&task_mapper(&1))
      on_response ->
        for n <- notification, do: send_push(n, on_response, opts)
    end
  end
  def push(notification, opts) do
    case opts[:on_response] do
      nil -> sync_push(notification, opts)
      on_response -> send_push(notification, on_response, opts)
    end
  end

  defp task_mapper({task, result}) do
    case result do
      nil -> Task.shutdown(task, :brutal_kill)
      {:ok, {:ok, response}} -> {:ok, response}
      {:ok, {:error, :timeout, notification}} -> {:error, notification}
    end
  end

  def send_push(notification, on_response, opts) do
    worker_name = opts[:to] || @default_worker
    notification
    |> encode_requests()
    |> Enum.map(& GenServer.cast(worker_name, generate_envelope(&1, on_response, opts)))
  end

  defp sync_push(notification, opts) do
    ref = :erlang.make_ref
    pid = self()
    on_response = fn(x) -> send pid, {ref, x} end
    send_push(notification, on_response, opts)
    receive do
      {^ref, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end

  def encode_requests(%{registration_id: regid} = notification) when is_binary(regid) do
    encode_requests(%{notification | registration_id: [regid]})
  end
  def encode_requests(%{registration_id: regid} = notification) when length(regid) < 1001 do
    res =
      regid
      |> recipient_attr()
      |> Map.merge(notification.payload)
      |> Map.put("priority", to_string(notification.priority))
      |> Poison.encode!

    formatted_regid = regid
      |> List.wrap

    [{formatted_regid, res}]
  end
  def encode_requests(notification) do
    notification.registration_id
    |> chunk(@chunk_size, @chunk_size, [])
    |> Enum.map(& encode_requests(%{notification | registration_id: &1}))
    |> List.flatten
  end

  defp chunk(collection, chunk_size, step, padding) do
    if Kernel.function_exported?(Enum, :chunk_every, 4) do
      Enum.chunk_every(collection, chunk_size, step, padding)
    else
      Enum.chunk(collection, chunk_size, step, padding)
    end
  end

  defp recipient_attr([regid]), do: %{"to" => regid}
  defp recipient_attr(regid) when is_list(regid), do: %{"registration_ids" => regid}

  @doc ~S"""
  Starts FCM worker connection with given config or name.

  ## Examples

      iex> config = Pigeon.FCM.Config.new(:fcm_default)
      iex> {:ok, pid} = Pigeon.FCM.start_connection(%{config | name: nil})
      iex> is_pid(pid)
      true
  """
  def start_connection(opts \\ [])
  def start_connection(name) when is_atom(name) do
    worker = worker(Pigeon.Worker, [Config.new(name)], id: name)
    Supervisor.start_child(:pigeon, worker)
  end
  def start_connection(%Config{} = config) do
    Pigeon.Worker.start_link(config)
  end
  def start_connection(opts) do
    opts
    |> Config.new
    |> start_connection()
  end

  @doc ~S"""
  Stops existing FCM worker connection.

  ## Examples

      iex> config = Pigeon.FCM.Config.new(:fcm_default)
      iex> {:ok, pid} = Pigeon.FCM.start_connection(%{config | name: nil})
      iex> Pigeon.FCM.stop_connection(pid)
      :ok
  """
  @spec stop_connection(atom | pid) :: :ok
  def stop_connection(name), do: Pigeon.Worker.stop_connection(name)

  def generate_envelope(payload, on_response, opts) do
    opts = Keyword.put(opts, :on_response, on_response)
    {:push, payload, opts}
  end

  def merge(response_1, response_2) do
    Map.merge(response_1, response_2, fn(key, value_1, value_2) ->
      cond do
        key == :__struct__ -> value_1
        is_map(value_1) -> merge(value_1, value_2)
        is_nil(value_1) -> value_2
        is_nil(value_2) -> value_1
        is_list(value_1) && is_list(value_2) -> value_1 ++ value_2
        true -> [value_1] ++ [value_2]
      end
    end)
  end
end
