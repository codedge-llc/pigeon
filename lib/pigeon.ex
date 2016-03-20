defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  A wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args), do: Pigeon.Supervisor.start_link

  defmodule APNS do
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

  defmodule GCM do
    @doc """
      Sends a push over GCM
    """
    @spec push(Pigeon.GCM.Notification) :: none
    def push(notification), do: Pigeon.Supervisor.push(:gcm, notification)

    @doc """
      Sends a push over GCM and executes function on success/failure.
    """
    @spec push(Pigeon.GCM.Notification, (() -> none)) :: none
    def push(notification, on_response), do: Pigeon.Supervisor.push(:gcm, notification, on_response)
  end
end
