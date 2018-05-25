defmodule Pigeon.FCM.ResultParserTest do
  use ExUnit.Case

  alias Pigeon.FCM.{Notification, ResultParser}

  def assert_response(notif, expected) do
    assert notif.response == expected
  end

  test "parse_result with success" do
    expected = [success: "regid"]

    ResultParser.parse(
      ["regid"],
      [%{"message_id" => "1:0408"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end

  test "parse_result with single non-list regid" do
    expected = [success: "regid"]

    ResultParser.parse(
      "regid",
      [%{"message_id" => "1:0408"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end

  test "parse_result with success and new registration_id" do
    pid = self()
    resp = fn resp -> send(pid, resp) end

    ResultParser.parse(
      ["regid"],
      [%{"message_id" => "1:2342", "registration_id" => "32"}],
      &resp.(&1),
      %Notification{}
    )

    receive do
      notif ->
        assert notif.response == [update: {"regid", "32"}]
        assert notif.message_id == "1:2342"
    after
      5_000 -> flunk("No response received.")
    end
  end

  test "parse_result with error Unavailable" do
    expected = [unavailable: "regid"]

    ResultParser.parse(
      ["regid"],
      [%{"error" => "Unavailable"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end

  test "parse_result with error NotRegistered" do
    expected = [not_registered: "regid"]

    ResultParser.parse(
      ["regid"],
      [%{"error" => "NotRegistered"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end

  test "parse_result with error InvalidRegistration" do
    expected = [invalid_registration: "regid"]

    ResultParser.parse(
      ["regid"],
      [%{"error" => "InvalidRegistration"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end

  test "parse_result with custom error" do
    expected = [custom_error: "regid"]

    ResultParser.parse(
      ["regid"],
      [%{"error" => "CustomError"}],
      &assert_response(&1, expected),
      %Notification{}
    )
  end
end
