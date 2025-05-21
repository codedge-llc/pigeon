defmodule Pigeon.FCM.ErrorTest do
  use ExUnit.Case, async: true

  test "parse/1 handles error reasons via status" do
    reasons = [
      {"INTERNAL", :internal},
      {"INVALID_ARGUMENT", :invalid_argument},
      {"PERMISSION_DENIED", :permission_denied},
      {"QUOTA_EXCEEDED", :quota_exceeded},
      {"SENDER_ID_MISMATCH", :sender_id_mismatch},
      {"THIRD_PARTY_AUTH_ERROR", :third_party_auth_error},
      {"UNAVAILABLE", :unavailable},
      {"UNREGISTERED", :unregistered},
      {"UNSPECIFIED_ERROR", :unspecified_error}
    ]

    for {actual, expected} <- reasons do
      payload = %{"status" => actual}
      assert Pigeon.FCM.Error.parse(payload) == expected
    end
  end

  test "parse/1 handles error reasons via errorCode" do
    reasons = [
      {"INTERNAL", :internal},
      {"INVALID_ARGUMENT", :invalid_argument},
      {"PERMISSION_DENIED", :permission_denied},
      {"QUOTA_EXCEEDED", :quota_exceeded},
      {"SENDER_ID_MISMATCH", :sender_id_mismatch},
      {"THIRD_PARTY_AUTH_ERROR", :third_party_auth_error},
      {"UNAVAILABLE", :unavailable},
      {"UNREGISTERED", :unregistered},
      {"UNSPECIFIED_ERROR", :unspecified_error}
    ]

    for {actual, expected} <- reasons do
      payload = %{"details" => [%{"errorCode" => actual}]}
      assert Pigeon.FCM.Error.parse(payload) == expected
    end
  end

  test "parse/1 handles unknown error reasons via status" do
    payload = %{"status" => "UNKNOWN_ERROR_CODE"}
    assert Pigeon.FCM.Error.parse(payload) == :unknown_error
  end

  test "parse/1 handles unknown error reasons via errorCode" do
    payload = %{"details" => [%{"errorCode" => "UNKNOWN_ERROR_CODE"}]}
    assert Pigeon.FCM.Error.parse(payload) == :unknown_error
  end
end
