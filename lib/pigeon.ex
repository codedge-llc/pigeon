defmodule Pigeon do
  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  @default_timeout 5_000

  @typedoc ~S"""
  Async callback for push notifications response.

  ## Examples

      handler = fn(%Pigeon.ADM.Notification{response: response}) ->
        case response do
          :success ->
            Logger.debug "Push successful!"
          :unregistered ->
            Logger.error "Bad device token!"
          _error ->
            Logger.error "Some other error happened."
        end
      end

      n = Pigeon.ADM.Notification.new("token", %{"message" => "test"})
      Pigeon.ADM.push(n, on_response: handler)
  """
  @type on_response :: (Notification.t() -> no_return)

  @typedoc ~S"""
  Options for sending push notifications.

  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  - `:timeout` - Push timeout. Defaults to 5000ms.
  """
  @type push_opts :: [
          on_response: on_response | nil,
          timeout: non_neg_integer
        ]

  @doc """
  Returns the configured JSON encoding library for Pigeon.
  To customize the JSON library, include the following in your config/config.exs:

      config :pigeon, :json_library, Jason
  """
  @spec json_library :: module
  def json_library do
    Application.get_env(:pigeon, :json_library, Jason)
  end

  def debug_log?, do: Application.get_env(:pigeon, :debug_log, false)

  @spec push(pid | atom, notification :: struct | [struct], push_opts) ::
          {:ok, notification :: struct}
          | {:error, notification :: struct}
          | :ok
  def push(pid, notifications, opts \\ [])

  def push(pid, notifications, opts) when is_list(notifications) do
    if Keyword.has_key?(opts, :on_response) do
      push_async(pid, notifications, opts[:on_response])
    else
      timeout = Keyword.get(opts, :timeout, @default_timeout)

      notifications
      |> Enum.map(&Task.async(fn -> push_sync(pid, &1, timeout) end))
      |> Task.yield_many(timeout + 500)
      |> Enum.map(fn {task, response} ->
        case response do
          nil -> Task.shutdown(task, :brutal_kill)
          {:ok, resp} -> resp
          _error -> nil
        end
      end)
    end
  end

  def push(pid, notification, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    if Keyword.has_key?(opts, :on_response) do
      push_async(pid, notification, opts[:on_response])
    else
      push_sync(pid, notification, timeout)
    end
  end

  defp push_sync(pid, notification, timeout) do
    myself = self()
    ref = :erlang.make_ref()
    on_response = fn x -> send(myself, {:"$push", ref, x}) end

    push_async(pid, notification, on_response)

    receive do
      {:"$push", ^ref, x} -> x
    after
      timeout -> %{notification | response: :timeout}
    end
  end

  defp push_async(pid, notifications, on_response)
       when is_list(notifications) do
    for n <- notifications, do: push_async(pid, n, on_response)
  end

  defp push_async(pid, notification, nil) do
    GenServer.cast(pid, {:push, notification, nil})
  end

  defp push_async(pid, notification, on_response) when is_pid(pid) do
    if Process.alive?(pid) do
      GenServer.cast(pid, {:push, notification, on_response})
    else
      on_response.(%{notification | response: :not_started})
    end
  end

  defp push_async(pid, notification, on_response) do
    case Process.whereis(pid) do
      nil ->
        on_response.(%{notification | response: :not_started})

      _pid ->
        GenServer.cast(pid, {:push, notification, on_response})
    end
  end
end
