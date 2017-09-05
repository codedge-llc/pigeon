defmodule Pigeon.ADM do
  @moduledoc """
  Amazon Device Messaging (ADM)
  """

  alias Pigeon.ADM.{Config, Notification, NotificationResponse}

  @default_timeout 5_000

  @doc """
  Sends a push over ADM.

  ## Examples

      iex> msg = %{ "body" => "your message" }
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> Pigeon.ADM.push(n, on_response: fn(x) -> IO.inspect(x) end)
      :ok

      iex> msg = %{ "body" => "your message" }
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> Pigeon.ADM.push(n)
      {:ok, %Pigeon.ADM.NotificationResponse{remove: ["your_reg_id"]}}
  """
  @spec push(Notification.t | [Notification.t], Keyword.t) :: no_return
  def push(notifications, opts \\ [])
  def push(notifications, opts) when is_list(notifications) do
    worker_name = opts[:to] || Config.default_name
    case opts[:on_response] do
      nil ->
        notifications
        |> Enum.map(& Task.async(fn -> sync_push(worker_name, &1) end))
        |> Task.yield_many(@default_timeout + 500)
        |> Enum.map(fn {task, response} ->
            response || Task.shutdown(task, :brutal_kill)
           end)
        |> NotificationResponse.new
      on_response -> cast_push(worker_name, notifications, on_response)
    end
  end
  def push(notification, opts) do
    worker_name = opts[:to] || Config.default_name
    case opts[:on_response] do
      nil -> sync_push(worker_name, notification)
      on_response -> cast_push(worker_name, notification, on_response)
    end
  end

  defp cast_push(worker_name, notification, nil) do
    GenServer.cast(worker_name, {:push, :adm, notification})
  end
  defp cast_push(worker_name, notification, on_response) do
    GenServer.cast(worker_name, {:push, :adm, notification, on_response})
  end

  defp sync_push(worker_name, notification) do
    pid = self()
    ref = :erlang.make_ref
    on_response = fn(x) -> send pid, {ref, x} end

    GenServer.cast(worker_name, {:push, :adm, notification, on_response})

    receive do
      {^ref, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end
end
