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
        ref = :erlang.make_ref
        pid = self()
        for n <- notification do
          on_response = fn(x) -> send pid, {ref, x} end
          send_push(n, on_response, opts)
        end
        Enum.foldl(notification, %{}, fn(n, acc) ->
          receive do
            {^ref, %NotificationResponse{message_id: id} = response} ->
              if Map.has_key?(acc, id) do
                %{acc | id => merge(response, acc[:message_id])}
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
        |> Poison.encode!
        formatted_regid = regid
        |> List.wrap

      [{formatted_regid, res}]
  end

  def encode_requests(notification) do
      notification.registration_id
      |> Enum.chunk(1000, 1000, [])
      |> Enum.map(& encode_requests(%{notification | registration_id: &1}))
      |> List.flatten
  end

  defp recipient_attr([regid]), do: %{"to" => regid}
  defp recipient_attr(regid) when is_list(regid), do: %{"registration_ids" => regid}

  @doc """
    Sends a push over GCM.
  """
  def send_push(notification, on_response, opts) do
    notification
    |> encode_requests()
    |> Enum.map(& GenServer.cast(:gcm_worker, generate_envelope(&1, on_response, opts)))
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

  def generate_envelope(payload, on_response, opts) do
    {:push, :gcm, payload, on_response, Map.new(opts)}
  end

  # def merge(%NotificationResponse{ok: ok1,
  #                                 retry: retry1,
  #                                 update: update1,
  #                                 remove: remove1,
  #                                 error: error1}, %NotificationResponse{ok: ok2,
  #                                                                       retry: retry2,
  #                                                                       update: update2,
  #                                                                       remove: remove2,
  #                                                                       error: error2}) do

  #   error3 = Map.merge(error1, error2, fn(m, a, b) ->  a ++ b end)
  #   %NotificationResponse{
  #     ok: ok1 ++ ok2,
  #     retry: retry1 ++ retry2,
  #     update: update1 ++ update2,
  #     remove: remove1 ++ remove2,
  #     error: error3
  #   }
  # end

  def merge(response_1, response_2) do
    Map.merge(response_1, response_2, fn(key, value_1, value_2) ->
      cond do
        key == :__struct__ -> value_1
        is_map(value_1) -> merge(value_1, value_2)
        is_nil(value_1) -> value_2
        is_nil(value_2) -> value_1
        true -> value_1 ++ value_2
      end
    end)
  end
end
