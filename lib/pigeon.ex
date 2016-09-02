defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    response = Pigeon.Supervisor.start_link

    :pigeon
    |> Application.get_env(:apns)
    |> Enum.each(fn({pool, config}) -> Pigeon.APNS.start_connection(pool) end)

    response
  end
end
