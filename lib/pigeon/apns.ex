defmodule Pigeon.APNS do
  @doc """
    Sends a push over APNS.
  """
  @spec push(Pigeon.APNS.Notification) :: none
  def push(notification), do: Pigeon.Supervisor.push(:apns, notification)

  @doc """
    Sends a push over APNS.
  """
  @spec push(Pigeon.APNS.Notification, (() -> none)) :: none
  def push(notification, on_response), do: Pigeon.Supervisor.push(:apns, notification, on_response)
end
