defmodule Pigeon.Sandbox do
  @moduledoc """
  Sandbox adapter for any kind of push notification.

  If the push notification has `response: nil`, it will be marked
  with `response: :success`. Otherwise it will return the push notification
  exactly as given.
  """

  import Pigeon.Tasks, only: [process_on_response: 1]

  @behaviour Pigeon.Adapter

  @impl true
  def init(opts \\ []) do
    {:ok, opts}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_push(%{response: nil} = notification, state) do
    process_on_response(%{notification | response: :success})
    {:noreply, state}
  end

  def handle_push(notification, state) do
    process_on_response(notification)
    {:noreply, state}
  end
end
