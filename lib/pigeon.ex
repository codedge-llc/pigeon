defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  A wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    Pigeon.Server.start_link
  end

  defmodule APNS do
    def push(notification) do
      Pigeon.Server.push(:apns, notification)
    end
  end

  defmodule GCM do
    import HTTPoison

    def push(notification, auth_key) do
      url = 'https://gcm-http.googleapis.com/gcm/send'
      headers =  [{ "Authorization", "key=#{auth_key}" },
                  { "Content-Type", "application/json" },
                  { "Accept", "application/json" }]
    end
  end
end
