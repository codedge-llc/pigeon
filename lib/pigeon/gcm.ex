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

    payload = Map.merge(%{"to" => notification.registration_id}, notification.payload)

    GenServer.cast(:gcm_worker, {:push, :gcm, payload, on_response})

    receive do
      {:ok, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end

  #defp do_push(notification, %{gcm_key: gcm_key}, on_response \\ nil) do
    
#
  #  response =
  #    case on_response do
  #      nil ->
  #        fn({_reg_ids, payload}) ->
  #          HTTPoison.post(gcm_uri(), payload, gcm_headers(gcm_key))
  #        end
  #      _ ->
  #        fn({reg_ids, payload}) ->
  #          {:ok, %HTTPoison.Response{status_code: status, body: body}} =
  #            HTTPoison.post(gcm_uri(), payload, gcm_headers(gcm_key))
#
  #          notification = %{ notification | registration_id: reg_ids }
  #          process_response(status, body, notification, on_response)
  #        end
  #    end
  #  for r <- requests, do: Task.async(fn -> response.(r) end)
  #  :ok
  #end
#
  #def chunk_registration_ids(reg_ids) when is_binary(reg_ids), do: [[reg_ids]]
  #def chunk_registration_ids(reg_ids), do: Enum.chunk(reg_ids, 1000, 1000, [])

  def encode_requests([[reg_id]|_rest], payload) do
    to_send = Map.merge(%{"to" => reg_id}, payload)
    [{reg_id, Poison.encode!(to_send)}]
  end
  def encode_requests(registration_ids, payload) do
    Enum.map(registration_ids, fn(x) -> encode_payload(x, payload) end)
  end

  defp encode_payload(x, payload) do
    encoded =
      %{"registration_ids" => x}
      |> Map.merge(payload)
      |> Poison.encode!
    {x, encoded}
  end

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
    for n <- notification, do: push(n, on_response, opts)
  end
  def push(notification, on_response, opts) do
    GenServer.cast(:gcm_worker, {:push, :gcm, notification, on_response})
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

  def pmap(collection, func) do
      collection
      |> Enum.map(&(Task.async(fn -> func.(&1) end)))
      |> Enum.map(&Task.await/1)
    end
end
