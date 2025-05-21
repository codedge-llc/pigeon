defmodule Pigeon.APNS.ErrorTest do
  use ExUnit.Case, async: true

  test "parse/1 handles error reasons" do
    reasons = [
      {"BadCertificate", :bad_certificate},
      {"BadCertificateEnvironment", :bad_certificate_environment},
      {"BadCollapseId", :bad_collapse_id},
      {"BadDeviceToken", :bad_device_token},
      {"BadExpirationDate", :bad_expiration_date},
      {"BadMessageId", :bad_message_id},
      {"BadPath", :bad_path},
      {"BadPriority", :bad_priority},
      {"BadTopic", :bad_topic},
      {"DeviceTokenNotForTopic", :device_token_not_for_topic},
      {"DuplicateHeaders", :duplicate_headers},
      {"ExpiredProviderToken", :expired_provider_token},
      {"ExpiredToken", :expired_token},
      {"Forbidden", :forbidden},
      {"IdleTimeout", :idle_timeout},
      {"InternalServerError", :internal_server_error},
      {"InvalidProviderToken", :invalid_provider_token},
      {"InvalidPushType", :invalid_push_type},
      {"MethodNotAllowed", :method_not_allowed},
      {"MissingDeviceToken", :missing_device_token},
      {"MissingProviderToken", :missing_provider_token},
      {"MissingTopic", :missing_topic},
      {"PayloadEmpty", :payload_empty},
      {"PayloadTooLarge", :payload_too_large},
      {"ServiceUnavailable", :service_unavailable},
      {"Shutdown", :shutdown},
      {"TooManyProviderTokenUpdates", :too_many_provider_token_updates},
      {"TooManyRequests", :too_many_requests},
      {"TopicDisallowed", :topic_disallowed},
      {"Unregistered", :unregistered}
    ]

    for {actual, expected} <- reasons do
      payload = JSON.encode!(%{"reason" => actual})
      assert Pigeon.APNS.Error.parse(payload) == expected
    end
  end

  test "parse/1 handles unknown error reasons" do
    payload = JSON.encode!(%{"reason" => "UnknownReason"})
    assert Pigeon.APNS.Error.parse(payload) == :unknown_error
  end
end
