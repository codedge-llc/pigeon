defmodule Pigeon.FCM.ResultParserTest do
  use ExUnit.Case

  alias Pigeon.FCM.{NotificationResponse, ResultParser}

  test "parse_result with success" do
    {:ok, response} =
      ResultParser.parse(
        ["regid"],
        [%{ "message_id" => "1:0408" }],
        &(&1), %NotificationResponse{}
      )
    assert  response.ok == ["regid"]
  end

  test "parse_result with success and new registration_id" do
    {:ok, response} =
      ResultParser.parse(
        ["regid"],
        [%{ "message_id" => "1:2342", "registration_id" => "32" }],
        &(&1), %NotificationResponse{}
      )

    assert response.update == [{"regid", "32"}]
    assert response.message_id == "1:2342"
  end

  test "parse_result with error unavailable" do
    {:ok, response} =
      ResultParser.parse(
        ["regid"],
        [%{ "error" => "Unavailable" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.retry == ["regid"]
  end

  test "parse_result with custom error" do
    {:ok, response} =
      ResultParser.parse(
        ["regid"],
        [%{ "error" => "CustomError" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.error == %{"CustomError" => "regid"}
  end
end
