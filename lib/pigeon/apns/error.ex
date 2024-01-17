defmodule Pigeon.APNS.Error do
  @moduledoc false

  require Logger

  alias Pigeon.APNS.Notification

  @doc false
  @spec parse(binary) :: Notification.error_response()
  def parse(data) do
    data
    |> Pigeon.json_library().decode!()
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

  defp parse_response("InvalidPushType"), do: :invalid_push_type

  defp parse_response("MissingDeviceToken"), do: :missing_device_token

  defp parse_response("MissingTopic"), do: :missing_topic

  defp parse_response("PayloadEmpty"), do: :payload_empty

  defp parse_response("TopicDisallowed"), do: :topic_disallowed

  defp parse_response("BadCertificate"), do: :bad_certificate

  defp parse_response("BadCertificateEnvironment"),
    do: :bad_certificate_environment

  defp parse_response("ExpiredProviderToken"), do: :expired_provider_token

  defp parse_response("Forbidden"), do: :forbidden

  defp parse_response("InvalidProviderToken"), do: :invalid_provider_token

  defp parse_response("MissingProviderToken"), do: :missing_provider_token

  defp parse_response("BadPath"), do: :bad_path

  defp parse_response("MethodNotAllowed"), do: :method_not_allowed

  defp parse_response("ExpiredToken"), do: :expired_token

  defp parse_response("Unregistered"), do: :unregistered

  defp parse_response("PayloadTooLarge"), do: :payload_too_large

  defp parse_response("TooManyProviderTokenUpdates"),
    do: :too_many_provider_token_updates

  defp parse_response("TooManyRequests"), do: :too_many_requests

  defp parse_response("InternalServerError"), do: :internal_server_error

  defp parse_response("ServiceUnavailable"), do: :service_unavailable

  defp parse_response("Shutdown"), do: :shutdown

  defp parse_response(_), do: :unknown_error
end
