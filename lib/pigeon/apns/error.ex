defmodule Pigeon.APNS.Error do
  @moduledoc ~S"""
  Defines APNS error responses
  """

  require Logger

  alias Pigeon.APNS.Notification

  @type error_response :: :bad_collapse_id
                        | :bad_device_token
                        | :bad_expiration_date
                        | :bad_message_id
                        | :bad_priority
                        | :bad_topic
                        | :device_token_not_for_topic
                        | :duplicate_headers
                        | :idle_timeout
                        | :missing_device_token
                        | :missing_topic
                        | :payload_empty
                        | :topic_disallowed
                        | :bad_certificate
                        | :bad_certificate_environment
                        | :expired_provider_token
                        | :forbidden
                        | :invalid_provider_token
                        | :missing_provider_token
                        | :bad_path
                        | :method_not_allowed
                        | :unregistered
                        | :payload_too_large
                        | :too_many_provider_token_updates
                        | :too_many_requests
                        | :internal_server_error
                        | :service_unavailable
                        | :shutdown

  @doc false
  @spec parse(binary) :: error_response
  def parse(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  @doc ~S"""
  If enabled, logs a notification and its error response.
  """
  @spec log(error_response, Notification.t) :: :ok
  def log(reason, notification) do
    if Pigeon.debug_log? do
      Logger.error("#{reason}: #{msg(reason)}\n#{inspect(notification)}")
    end
  end

  @doc false
  @spec msg(error_response) :: String.t
  # 400
  def msg(:bad_collapse_id) do
    "The collapse identifier exceeds the maximum allowed size"
  end
  def msg(:bad_device_token) do
    """
    The specified device token was bad. Verify that the request contains a
    valid token and that the token matches the environment.
    """
  end
  def msg(:bad_expiration_date), do: "The apns-expiration value is bad."
  def msg(:bad_message_id), do: "The apns-id value is bad."
  def msg(:bad_priority), do: "The apns-priority value is bad."
  def msg(:bad_topic), do: "The apns-topic was invalid."
  def msg(:device_token_not_for_topic) do
    "The device token does not match the specified topic."
  end
  def msg(:duplicate_headers), do: "One or more headers were repeated."
  def msg(:idle_timeout), do: "Idle time out."
  def msg(:missing_device_token) do
    """
    The device token is not specified in the request :path. Verify that the
    :path header contains the device token.
    """
  end
  def msg(:missing_topic) do
    """
    The apns-topic header of the request was not specified and was required.
    The apns-topic header is mandatory when the client is connected using a
    certificate that supports multiple topics.
    """
  end
  def msg(:payload_empty), do: "The message payload was empty."
  def msg(:topic_disallowed), do: "Pushing to this topic is not allowed."

  # 403
  def msg(:bad_certificate), do: "The certificate was bad."
  def msg(:bad_certificate_environment) do
    "The client certificate was for the wrong environment."
  end
  def msg(:expired_provider_token) do
    "The provider token is stale and a new token should be generated."
  end
  def msg(:forbidden) do
    "The specified action is not allowed."
  end
  def msg(:invalid_provider_token) do
    """
    The provider token is not valid or the token signature could not
    be verified."
    """
  end
  def msg(:missing_provider_token) do
    """
    No provider certificate was used to connect to APNs and Authorization
    header was missing or no provider token was specified."
    """
  end

  # 404
  def msg(:bad_path), do: "The request contained a bad :path value."

  # 405
  def msg(:method_not_allowed), do: "The specified :method was not POST."

  # 410
  def msg(:unregistered) do
    "The device token is inactive for the specified topic."
  end

  # 413
  def msg(:payload_too_large) do
    """
    The message payload was too large. The maximum payload size is
    4096 bytes.
    """
  end
  # 429
  def msg(:too_many_provider_token_updates) do
    "The provider token is being updated too often."
  end
  def msg(:too_many_requests) do
    "Too many requests were made consecutively to the same device token."
  end

  # 500
  def msg(:internal_server_error), do: "An internal server error occurred."

  # 503
  def msg(:service_unavailable), do: "The service is unavailable."
  def msg(:shutdown), do: "The server is shutting down."

  # Misc
  def msg(:timeout), do: "The SSL connection timed out."
  def msg(_else), do: ""
end
