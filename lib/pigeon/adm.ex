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
      iex> Pigeon.ADM.push(n)
      :ok

      iex> msg = %{ "body" => "your message" }
      iex> n = Pigeon.ADM.Notification.new("your_reg_id", msg)
      iex> me = self()
      iex> handler = fn(x) -> send(me, x) end
      iex> Pigeon.ADM.push(n, on_response: handler)
      iex> receive do
      ...>   x -> x
      ...> after
      ...>   5_000 -> "Nothing received"
      ...> end
      {:error, :invalid_registration_id,
      %Pigeon.ADM.Notification{consolidation_key: nil,
      expires_after: 604800, md5: "M13RuG4uDWqajseQcCiyiw==",
      payload: %{"data" => %{"body" => "your message"}},
      registration_id: "your_reg_id", updated_registration_id: nil}}
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
