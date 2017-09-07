defmodule Pigeon.FCM.ResultParserTest do
  use ExUnit.Case

  alias Pigeon.FCM.{Notification, ResultParser}

  test "parse_result with success" do
    notif = ResultParser.parse(["regid"],
                               [%{"message_id" => "1:0408"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [success: "regid"]
  end

  test "parse_result with single non-list regid" do
    notif = ResultParser.parse("regid",
                               [%{"message_id" => "1:0408"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [success: "regid"]
  end

  test "parse_result with success and new registration_id" do
    notif =
      ResultParser.parse(
        ["regid"],
        [%{"message_id" => "1:2342", "registration_id" => "32"}],
        &(&1), %Notification{}
      )

    assert notif.response == [update: {"regid", "32"}]
    assert notif.message_id == "1:2342"
  end

  test "parse_result with error Unavailable" do
    notif = ResultParser.parse(["regid"],
                               [%{"error" => "Unavailable"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [unavailable: "regid"]
  end

  test "parse_result with error NotRegistered" do
    notif = ResultParser.parse(["regid"],
                               [%{"error" => "NotRegistered"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [not_registered: "regid"]
  end

  test "parse_result with error InvalidRegistration" do
    notif = ResultParser.parse(["regid"],
                               [%{"error" => "InvalidRegistration"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [invalid_registration: "regid"]
  end

  test "parse_result with custom error" do
    notif = ResultParser.parse(["regid"],
                               [%{"error" => "CustomError"}],
                               &(&1),
                               %Notification{})
    assert notif.response == [custom_error: "regid"]
  end
end
