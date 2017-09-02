defmodule Pigeon.APNS do
  @moduledoc """
  Defines publically-exposed Apple Push Notification Service (APNS) functions. For implementation
  see APNS.Worker.
  """
  require Logger
  import Supervisor.Spec

  alias Pigeon.APNS.{Config, Notification}

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
    ref = :erlang.make_ref
    on_response = fn(x) -> send pid, {ref, x} end

    worker_name = opts[:to] || Config.default_name
    cast_push(worker_name, notification, on_response: on_response)

    receive do
      {^ref, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end

  def cast_push(pid, notification, opts) do
    GenServer.cast(pid, {:push, notification, opts})
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
  Sends a push over APNS.
  """
  @spec push([Notification.t], ((Notification.t) -> no_return), Keyword.t) :: no_return
  def push(notification, on_response, opts) when is_list(notification) do
    for n <- notification, do: push(n, on_response, opts)
  end

  @spec push(Notification.t, ((Notification.t) -> no_return), Keyword.t) :: no_return
  def push(notification, on_response, opts) do
    worker_name = opts[:to] || Config.default_name
    cast_push(worker_name, notification, on_response: on_response)
  end

  @spec start_connection(atom | Keyword.t) :: {:ok, pid}
  def start_connection(opts \\ [])
  def start_connection(name) when is_atom(name) do
    config = Config.config(name)
    Supervisor.start_child(:pigeon, worker(Pigeon.Worker, [config], id: name))
  end
  def start_connection(opts) do
    config = %Pigeon.APNS.Config{
      name: opts[:name],
      mode: opts[:mode],
      cert: Config.cert(opts[:cert]),
      certfile: Config.file_path(opts[:cert]),
      key: Config.key(opts[:key]),
      keyfile: Config.file_path(opts[:key]),
      ping_period: opts[:ping_period] || 600_000
    }
    Pigeon.Worker.start_link(config)
  end

  def stop_connection(name) do
    Supervisor.terminate_child(:pigeon, name)
    Supervisor.delete_child(:pigeon, name)
  end
end
