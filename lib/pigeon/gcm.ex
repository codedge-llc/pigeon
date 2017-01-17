defmodule Pigeon.GCM do
  @moduledoc """
  Handles all Google Cloud Messaging (GCM) request and response functionality.
  """
  require Logger
  import Supervisor.Spec

  alias Pigeon.GCM.NotificationResponse
  alias Pigeon.GCM.Notification

  @default_timeout 5_000

  def push(notification, opts \\ [])
  def push(notification, opts) when is_list(notification) do
    case opts[:on_response] do
      nil ->
        for n <- notification do
          pid = self()
          on_response = fn(x) -> send pid, {:ok, x} end
          send_push(notification, on_response, opts)
        end
        Enum.foldl(notification, %{}, fn(n, acc) ->
          receive do
            {:ok, %NotificationResponse{message_id: id} = response} ->
              if   Map.has_key? acc, id do
                %{ acc | id => merge(response, acc[:message_id])}
              else
                Map.merge(%{id => response}, acc)
              end
          after 5_000 ->
            acc
          end
        end)
      on_response -> send_push(notification, on_response, opts)
    end
  end


  def push(notification, opts) do
    case opts[:on_response] do
      nil -> do_sync_push(notification, opts)
      on_response -> send_push(notification, on_response, opts)
    end
  end

  defp do_sync_push(notification, opts) do
    pid = self()
    on_response = fn(x) -> send pid, {:ok, x} end
    send_push(notification, on_response, opts)
    receive do
      {:ok, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end


  def encode_requests(%{registration_id: regid} = notification) when is_binary(regid) do
    encode_requests(%{notification | registration_id: [regid]})
  end
  def encode_requests(%{registration_id: regid} = notification) when length(regid) < 1001 do
      res = recipient_attr(regid)
      |> Map.merge(notification.payload)
      |> Poison.encode!
      formatted_regid = regid
      |> List.wrap
      [{  formatted_regid, res}]
  end

  def encode_requests(notification) do
      notification.registration_id
      |> Enum.chunk(1000, 1000, [])
      |> Enum.map(fn(chunk) ->
              encode_requests(%{notification | registration_id: chunk})
            end)
      |> List.flatten
  end


  defp recipient_attr([regid]), do: %{"to" => regid}
  defp recipient_attr(regid) when is_list(regid), do: %{"registration_ids" => regid}



  @doc """
    Sends a push over GCM.
  """
  def send_push(notification, on_response, opts) do
    encode_requests(notification)
    |> Enum.map( fn(payload) ->
          GenServer.cast(:gcm_worker, {:push, :gcm, payload, on_response})
        end)
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

  def merge %NotificationResponse{ok: ok1, retry: retry1, update: update1, remove: remove1, error: error1}, 
                   %NotificationResponse{ok: ok2, retry: retry2, update: update2, remove: remove2, error: error2} do
    error3 = Map.merge(error1, error2, fn(m, a, b) ->  a ++ b end)
    %NotificationResponse{ok: ok1 ++ ok2, 
                                          retry: retry1 ++ retry2, 
                                          update: update1 ++ update2, 
                                          remove: remove1 ++ remove2, 
                                          error: error3}
  end

end
