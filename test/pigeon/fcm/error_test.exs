defmodule Pigeon.FCM.ErrorTest do
  use ExUnit.Case, async: true
  alias Pigeon.FCM.Error

  describe "parse/1" do
    test "returns :unregistered when 'errorCode' is 'UNREGISTERED' in details" do
      error = %{
        "code" => 404,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "UNREGISTERED"
          },
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.ApnsError",
            "reason" => "Unregistered",
            "statusCode" => 410
          }
        ],
        "message" => "Requested entity was not found.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :unregistered
    end

    test "returns :invalid_argument when 'errorCode' is 'INVALID_ARGUMENT' in details" do
      error = %{
        "code" => 400,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "INVALID_ARGUMENT"
          }
        ],
        "message" => "Bad request.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :invalid_argument
    end

    test "returns :unknown_error when 'errorCode' is not found in details and 'status' is missing" do
      error = %{
        "code" => 500,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "reason" => "Unknown"
          }
        ],
        "message" => "Internal server error."
      }

      assert Error.parse(error) == :unknown_error
    end

    test "returns :quota_exceeded when 'errorCode' is 'QUOTA_EXCEEDED'" do
      error = %{
        "code" => 429,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "QUOTA_EXCEEDED"
          }
        ],
        "message" => "Quota exceeded.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :quota_exceeded
    end

    test "returns :unavailable when 'errorCode' is 'UNAVAILABLE'" do
      error = %{
        "code" => 503,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "UNAVAILABLE"
          }
        ],
        "message" => "Service unavailable.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :unavailable
    end

    test "returns :internal when 'errorCode' is 'INTERNAL'" do
      error = %{
        "code" => 500,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "INTERNAL"
          }
        ],
        "message" => "Internal server error.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :internal
    end

    test "returns :third_party_auth_error when 'errorCode' is 'THIRD_PARTY_AUTH_ERROR'" do
      error = %{
        "code" => 401,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "THIRD_PARTY_AUTH_ERROR"
          }
        ],
        "message" => "Third-party authentication error.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :third_party_auth_error
    end

    test "returns :permission_denied when 'errorCode' is 'PERMISSION_DENIED'" do
      error = %{
        "code" => 403,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "PERMISSION_DENIED"
          }
        ],
        "message" => "Permission denied.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :permission_denied
    end

    test "returns :unknown_error when 'errorCode' is unknown in details" do
      error = %{
        "code" => 400,
        "details" => [
          %{
            "@type" => "type.googleapis.com/google.firebase.fcm.v1.FcmError",
            "errorCode" => "UNKNOWN_ERROR"
          }
        ],
        "message" => "Unknown error.",
        "status" => "NOT_FOUND"
      }

      assert Error.parse(error) == :unknown_error
    end

    test "returns :invalid_argument when 'status' is 'INVALID_ARGUMENT'" do
      error = %{
        "code" => 400,
        "status" => "INVALID_ARGUMENT",
        "message" => "Bad request."
      }

      assert Error.parse(error) == :invalid_argument
    end

    test "returns :unknown_error when 'status' is not found" do
      error = %{
        "code" => 500,
        "message" => "Unknown error."
      }

      assert Error.parse(error) == :unknown_error
    end

    test "returns :unknown_error when 'status' is unknown" do
      error = %{
        "code" => 500,
        "status" => "UNKNOWN_STATUS",
        "message" => "Unknown status error."
      }

      assert Error.parse(error) == :unknown_error
    end
  end
end
