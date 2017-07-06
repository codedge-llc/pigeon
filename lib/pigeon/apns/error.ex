defmodule Pigeon.APNS.Error do
  @moduledoc false
  require Logger

  def parse(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  def log(reason, notification) do
    Logger.error("#{reason}: #{msg(reason)}\n#{inspect(notification)}")
  end

  def msg(error) do
    case error do
      # 400
      :bad_collapse_id ->
        "The collapse identifier exceeds the maximum allowed size"
      :bad_device_token ->
        """
        The specified device token was bad. Verify that the request contains a valid token and
        that the token matches the environment.
        """
      :bad_expiration_date ->
        "The apns-expiration value is bad."
      :bad_message_id ->
        "The apns-id value is bad."
      :bad_priority ->
        "The apns-priority value is bad."
      :bad_topic ->
        "The apns-topic was invalid."
      :device_token_not_for_topic ->
        "The device token does not match the specified topic."
      :duplicate_headers ->
        "One or more headers were repeated."
      :idle_timeout ->
        "Idle time out."
      :missing_device_token ->
        """
        The device token is not specified in the request :path. Verify that the :path header
        contains the device token.
        """
      :missing_topic ->
          """
          The apns-topic header of the request was not specified and was required. The apns-topic
          header is mandatory when the client is connected using a certificate that supports
          multiple topics.
          """
      :payload_empty ->
        "The message payload was empty."
      :topic_disallowed ->
        "Pushing to this topic is not allowed."

      # 403
      :bad_certificate ->
        "The certificate was bad."
      :bad_certificate_environment ->
        "The client certificate was for the wrong environment."
      :expired_provider_token ->
        "The provider token is stale and a new token should be generated."
      :forbidden ->
        "The specified action is not allowed."
      :invalid_provider_token ->
        "The provider token is not valid or the token signature could not be verified."
      :missing_provider_token ->
        """
        No provider certificate was used to connect to APNs and Authorization header was missing
        or no provider token was specified."
        """

      # 404
      :bad_path ->
        "The request contained a bad :path value."

      # 405
      :method_not_allowed ->
        "The specified :method was not POST."

      # 410
      :unregistered ->
        "The device token is inactive for the specified topic."

      # 413
      :payload_too_large ->
        "The message payload was too large. The maximum payload size is 4096 bytes."
      # 429
      :too_many_provider_token_updates ->
        "The provider token is being updated too often."
      :too_many_requests ->
        "Too many requests were made consecutively to the same device token."

      # 500
      :internal_server_error ->
        "An internal server error occurred."

      # 503
      :service_unavailable ->
        "The service is unavailable."
      :shutdown ->
        "The server is shutting down."

      # Misc
      :timeout ->
        "The SSL connection timed out."
      _ ->
        ""
    end
  end
end
