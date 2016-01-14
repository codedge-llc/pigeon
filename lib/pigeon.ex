defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  A wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    Pigeon.Supervisor.start_link
  end

  defmodule APNS do
    def push(notification) do
      Pigeon.Supervisor.push(:apns, notification)
    end
  end

  defmodule GCM do
    import HTTPoison

    def push(notification) do
      url = 'https://gcm-http.googleapis.com/gcm/send'
      headers =  [{ "Authorization", "key=#{Application.get_env(:pigeon, :gcm_key)}" },
                  { "Content-Type", "application/json" },
                  { "Accept", "application/json" }]

      HTTPoison.post!(url, notification, headers)
    end
  end
end
