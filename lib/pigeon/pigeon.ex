defmodule Pigeon do
  require Logger

  @moduledoc """
  Pigeon is a wrapper for sending iOS push notifications.
  """

  defmodule APNS do
    def push(notification, connection) do
      :ssl.send(connection.ssl_socket, notification)
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
