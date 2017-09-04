defmodule Pigeon.APNS do
  @moduledoc """
  Apple Push Notification Service (APNS)
  """

  require Logger
  import Supervisor.Spec

  alias Pigeon.APNS.{Config, Notification, NotificationResponse}

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

  ## Examples

     iex> n = Pigeon.APNS.Notification.new("msg", "token", "topic")
     iex> Pigeon.APNS.push(n)
     {:error, :bad_device_token,
       %Pigeon.APNS.Notification{device_token: "token", expiration: nil,
       id: nil, payload: %{"aps" => %{"alert" => "msg"}}, topic: "topic"}}

     iex> n = Pigeon.APNS.Notification.new("msg", "token", "topic")
     iex> Pigeon.APNS.push([n, n])
     %Pigeon.APNS.NotificationResponse{error: %{
       bad_device_token: [
         %Pigeon.APNS.Notification{device_token: "token", expiration: nil,
         id: nil, payload: %{"aps" => %{"alert" => "msg"}}, topic: "topic"},
         %Pigeon.APNS.Notification{device_token: "token", expiration: nil,
         id: nil, payload: %{"aps" => %{"alert" => "msg"}}, topic: "topic"}
     ]}, ok: []}
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
        |> NotificationResponse.new
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

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> {:ok, pid} = Pigeon.APNS.start_connection(%{config | name: nil})
      iex> is_pid(pid)
      true
  """
  @spec start_connection(atom | Config.t | Keyword.t) :: {:ok, pid}
  def start_connection(name) when is_atom(name) do
    config = Config.new(name)
    Supervisor.start_child(:pigeon, worker(Pigeon.Worker, [config], id: name))
  end
  def start_connection(%Config{} = config) do
    Pigeon.Worker.start_link(config)
  end
  def start_connection(opts) when is_list(opts) do
    opts
    |> Config.new
    |> start_connection()
  end

  @doc ~S"""
  Stops existing APNS worker connection.

  ## Examples

      iex> config = Pigeon.APNS.Config.new(:apns_default)
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
end
