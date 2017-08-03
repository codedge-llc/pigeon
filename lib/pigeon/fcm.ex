defmodule Pigeon.FCM do
  @moduledoc """
  Handles all Firebase Cloud Messaging (FCM) request and response functionality.
  """
  require Logger
  import Supervisor.Spec

  alias Pigeon.FCM.{Notification, NotificationResponse}

  @default_timeout 5_000
  @default_worker :fcm_default
  @chunk_size 1_000

  @spec push(Notification.t, Keyword.t) :: {:ok, NotificationResponse.t}
  @spec push([Notification.t, ...], Keyword.t) :: [NotificationResponse.t, ...]
  def push(notification, opts \\ [])
  def push(notification, opts) when is_list(notification) do
    case opts[:on_response] do
      nil ->
        tasks = for n <- notification, do: Task.async(fn -> do_sync_push(n, opts) end)
        tasks
        |> Task.yield_many(@default_timeout + 10_000)
        |> Enum.map(&task_mapper(&1))
      on_response ->
        for n <- notification, do: send_push(n, on_response, opts)
    end
  end
  def push(notification, opts) do
    case opts[:on_response] do
      nil -> do_sync_push(notification, opts)
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

  @doc """
  Sends a push over FCM.
  """
  def send_push(notification, on_response, opts) do
    worker_name = opts[:to] || @default_worker
    notification
    |> encode_requests()
    |> Enum.map(& GenServer.cast(worker_name, generate_envelope(&1, on_response, opts)))
  end

  defp do_sync_push(notification, opts) do
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

  def start_connection(opts \\ [])
  def start_connection(name) when is_atom(name) do
    config = Application.get_env(:pigeon, :fcm)[name]
    Supervisor.start_child(:pigeon, worker(Pigeon.FCM.Worker, [config], id: name))
  end
  def start_connection(opts) do
    config = %{
      name: opts[:name],
      key:  opts[:key],
      ping_period: opts[:ping_period]
    }
    Pigeon.FCM.Worker.start_link(config)
  end

  def stop_connection(name) do
    Supervisor.terminate_child(:pigeon, name)
    Supervisor.delete_child(:pigeon, name)
  end

  def generate_envelope(payload, on_response, opts) do
    {:push, :fcm, payload, on_response, Map.new(opts)}
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
