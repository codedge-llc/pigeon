defmodule Pigeon.APNS.Error do
  @moduledoc ~S"""
  Defines APNS error responses
  """

  require Logger

  alias Pigeon.APNS.Notification

  @type error_response ::
          :bad_collapse_id
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
          | :unknown_error

  @doc ~S"""
  If enabled, logs a notification and its error response.
  """
  @spec log(error_response, Notification.t()) :: :ok
  def log(reason, notification) do
    if Pigeon.debug_log?() do
      Logger.error("#{reason}: #{msg(reason)}\n#{inspect(notification)}")
    end
  end

  @doc false
  @spec parse(binary) :: error_response
  def parse(data) do
    data
    |> Poison.decode!()
    |> Map.get("reason")
    |> parse_response()
  end

  defp parse_response("BadCollapseId"), do: :bad_collapse_id
  defp parse_response("BadDeviceToken"), do: :bad_device_token
  defp parse_response("BadExpirationDate"), do: :bad_expiration_date
  defp parse_response("BadMessageId"), do: :bad_message_id
  defp parse_response("BadPriority"), do: :bad_priority
  defp parse_response("BadTopic"), do: :bad_topic
  defp parse_response("DeviceTokenNotForTopic"), do: :device_token_not_for_topic
  defp parse_response("DuplicateHeaders"), do: :duplicate_headers
  defp parse_response("IdleTimeout"), do: :idle_timeout
  defp parse_response("MissingDeviceToken"), do: :missing_device_token
  defp parse_response("MissingTopic"), do: :missing_topic
  defp parse_response("PayloadEmpty"), do: :payload_empty
  defp parse_response("TopicDisallowed"), do: :topic_disallowed
  defp parse_response("BadCertificate"), do: :bad_certificate
  defp parse_response("BadCertificateEnvironment"), do: :bad_certificate_environment
  defp parse_response("ExpiredProviderToken"), do: :expired_provider_token
  defp parse_response("Forbidden"), do: :forbidden
  defp parse_response("InvalidProviderToken"), do: :invalid_provider_token
  defp parse_response("MissingProviderToken"), do: :missing_provider_token
  defp parse_response("BadPath"), do: :bad_path
  defp parse_response("MethodNotAllowed"), do: :method_not_allowed
  defp parse_response("Unregistered"), do: :unregistered
  defp parse_response("PayloadTooLarge"), do: :payload_too_large
  defp parse_response("TooManyProviderTokenUpdates"), do: :too_many_provider_token_updates
  defp parse_response("TooManyRequests"), do: :too_many_requests
  defp parse_response("InternalServerError"), do: :internal_server_error
  defp parse_response("ServiceUnavailable"), do: :service_unavailable
  defp parse_response("Shutdown"), do: :shutdown
  defp parse_response(_), do: :unknown_error

  @doc false
  @spec msg(error_response) :: String.t()
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
