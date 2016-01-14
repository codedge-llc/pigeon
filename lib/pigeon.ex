defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  A wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args), do: Pigeon.Supervisor.start_link

  defmodule APNS do
    def push(notification), do: Pigeon.Supervisor.push(:apns, notification)
  end

  defmodule GCM do
    def push(notification), do: Pigeon.Supervisor.push(:gcm, notification)
  end
end
