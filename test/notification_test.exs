defmodule Pigeon.NotificationTest do
  use ExUnit.Case

  @reg_id "123456"

  test "FCM new with one registration_id" do
    expected_result = %Pigeon.FCM.Notification{
      registration_id: @reg_id,
      payload: %{}
    }

    assert Pigeon.FCM.Notification.new(@reg_id) == expected_result
  end

  test "FCM new with multiple registration_ids" do
    reg_ids = ["aaaaaa", "bbbbbb", "cccccc"]

    expected_result = %Pigeon.FCM.Notification{
      registration_id: reg_ids,
      payload: %{}
    }

    assert Pigeon.FCM.Notification.new(reg_ids) == expected_result
  end

  test "FCM new with notification map" do
    n = %{
      "body" => "test body",
      "title" => "Test Push",
      "icon" => "icon"
    }

    expected_result = %Pigeon.FCM.Notification{
      registration_id: @reg_id,
      payload: %{"notification" => n}
    }

    assert Pigeon.FCM.Notification.new(@reg_id, n) == expected_result
  end

  test "FCM new with data map" do
    data = %{
      "message" => "test"
    }

    expected_result = %Pigeon.FCM.Notification{
      registration_id: @reg_id,
      payload: %{"data" => data}
    }

    assert Pigeon.FCM.Notification.new(@reg_id, %{}, data) == expected_result
  end

  test "FCM new with notification and data maps" do
    n = %{
      "body" => "test body",
      "title" => "Test Push",
      "icon" => "icon"
    }

    data = %{
      "message" => "test"
    }

    expected_result = %Pigeon.FCM.Notification{
      registration_id: @reg_id,
      payload: %{"notification" => n, "data" => data}
    }

    assert Pigeon.FCM.Notification.new(@reg_id, n, data) == expected_result
  end
end
