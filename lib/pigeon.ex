defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    Pigeon.Supervisor.start_link
    Pigeon.APNS.start_default_connection
  end
end
