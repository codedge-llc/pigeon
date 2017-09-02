defmodule Pigeon.APNS do
  @moduledoc """
  Apple Push Notification Service (APNS).
  """

  require Logger
  import Supervisor.Spec

  alias Pigeon.APNS.{Config, Notification}

  @type notification :: Notification.t
                      | [Notification.t, ...]

  @typedoc ~S"""
  Async callback for push notifications response.

  ## Examples

      handler = fn(x) ->
        case x do
          {:ok, notification} ->
            Logger.debug "Push successful!"
          {:error, :bad_device_token, notification} ->
            Logger.error "Bad device token!"
          {:error, reason, notification} ->
            Logger.error "Some other error happened."
        end
      end
      n = Pigeon.APNS.Notification.new("msg", "device token", "push topic")
      Pigeon.APNS.push(n, on_response: handler)
  """
  @type on_response :: ((Notification.t) -> no_return)

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

  @doc """
  Sends a push over APNS.
  """
  @spec push(notification, push_opts) :: {:ok, term} | {:error, term, term}
  def push(notification, opts \\ [])
  def push(notification, opts) when is_list(notification) do
    case opts[:on_response] do
      nil ->
        tasks = for n <- notification, do: Task.async(fn -> sync_push(n, opts) end)
        tasks
        |> Task.yield_many(@default_timeout + 500)
        |> Enum.map(fn {task, response} -> response || Task.shutdown(task, :brutal_kill) end)
        |> group_responses
      on_response -> push(notification, on_response, opts)
    end
  end
  def push(notification, opts) do
    case opts[:on_response] do
      nil -> sync_push(notification, opts)
      on_response -> push(notification, on_response, opts)
    end
  end

  @spec push(notification, on_response, push_opts) :: no_return
  defp push(notification, on_response, opts) when is_list(notification) do
    for n <- notification, do: push(n, on_response, opts)
  end
  defp push(notification, on_response, opts) do
    worker_name = opts[:to] || Config.default_name
    Pigeon.Worker.cast_push(worker_name, notification, on_response: on_response)
  end

  @doc ~S"""
  Starts APNS worker connection with given config or name.

  ## Examples

      iex> config = Pigeon.APNS.Config.config(:apns_default)
      iex> {:ok, pid} = Pigeon.APNS.start_connection(%{config | name: nil})
      iex> is_pid(pid)
      true
  """
  @spec start_connection(atom | Config.t | Keyword.t) :: {:ok, pid}
  def start_connection(name) when is_atom(name) do
    config = Config.config(name)
    Supervisor.start_child(:pigeon, worker(Pigeon.Worker, [config], id: name))
  end
  def start_connection(%Pigeon.APNS.Config{} = config) do
    Pigeon.Worker.start_link(config)
  end
  def start_connection(opts) do
    start_connection(
      %Pigeon.APNS.Config{
        name: opts[:name],
        mode: opts[:mode],
        cert: Config.cert(opts[:cert]),
        certfile: Config.file_path(opts[:cert]),
        key: Config.key(opts[:key]),
        keyfile: Config.file_path(opts[:key]),
        ping_period: opts[:ping_period] || 600_000
      }
    )
  end

  @doc ~S"""
  Stops existing APNS worker connection.

  ## Examples

      iex> config = Pigeon.APNS.Config.config(:apns_default)
      iex> {:ok, pid} = Pigeon.APNS.start_connection(%{config | name: nil})
      iex> Pigeon.APNS.stop_connection(pid)
      :ok
  """
  @spec stop_connection(atom | pid) :: :ok
  def stop_connection(name), do: Pigeon.Worker.stop_connection(name)

  defp sync_push(notification, opts) do
    pid = self()
    ref = :erlang.make_ref
    on_response = fn(x) -> send pid, {ref, x} end

    worker_name = opts[:to] || Config.default_name
    Pigeon.Worker.cast_push(worker_name, notification, on_response: on_response)

    receive do
      {^ref, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
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
end
