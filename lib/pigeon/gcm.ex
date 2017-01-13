defmodule Pigeon.GCM do
  @moduledoc """
  Handles all Google Cloud Messaging (GCM) request and response functionality.
  """
  require Logger
  import Supervisor.Spec

  @default_timeout 5_000

  def push(notification, opts \\ [])
  def push(notification, opts) when is_list(notification) do
    case opts[:on_response] do
      nil ->
        tasks = for n <- notification, do: Task.async(fn -> do_sync_push(n, opts) end)
        tasks
        |> Task.yield_many(@default_timeout + 500)
        |> Enum.map(fn {task, response} -> response || Task.shutdown(task, :brutal_kill) end)
        |> group_responses
      on_response -> push(notification, on_response, opts)
    end
  end

  def push(notification, opts) do
    case opts[:on_response] do
      nil -> do_sync_push(notification, opts)
      on_response -> push(notification, on_response, opts)
    end
  end

  defp do_sync_push(notification, opts) do
    pid = self()
    on_response = fn(x) -> send pid, {:ok, x} end
    push(notification, on_response, opts)
    receive do
      {:ok, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end

  def encode_requests(notification) do
      regid = notification.registration_id
      res = %{attr_name(regid) => regid}
      |> Map.merge(notification.payload)
      |> Poison.encode!
      {regid, res}
  end

  defp attr_name(regid) when is_list(regid), do: "registration_ids"
  defp attr_name(regid) when is_binary(regid), do: "to"

  defp group_responses(responses) do
    Enum.reduce(responses, %{}, fn(response, acc) ->
      case response do
        {:ok, r} -> update_result(acc, r)
        _ -> acc
      end
    end)
  end

  defp update_result(acc, response) do
    case response do
      {:ok, notif} -> add_ok_notif(acc, notif)
      {:error, reason, notif} -> add_error_notif(acc, reason, notif)
    end
  end

  defp add_ok_notif(acc, notif) do
    oks = acc[:ok] || []
    Map.put(acc, :ok, oks ++ [notif])
  end

  defp add_error_notif(acc, reason, notif) do
    errors = acc[:error] || %{}
    similar = errors[reason] || []
    errors = Map.put(errors, reason, similar ++ [notif])
    Map.put(acc, :error, errors)
  end

  @doc """
    Sends a push over GCM.
  """
  def push(notification, on_response, opts) when is_list(notification) do
    for n <- notification do
      push(n, on_response, opts)
    end
  end

  def push(notification, on_response, opts) do
    payload = encode_requests(notification)
    GenServer.cast(:gcm_worker, {:push, :gcm, payload, on_response})
  end


  def start_connection(name) do
    config = %{
      name: name,
      gcm_key:  Application.get_env(:pigeon, :gcm)[:key]
    }
    Supervisor.start_child(:pigeon, worker(Pigeon.GCMWorker, [config], id: name))
  end

  def stop_connection(name) do
    Supervisor.terminate_child(:pigeon, name)
    Supervisor.delete_child(:pigeon, name)
  end

end
