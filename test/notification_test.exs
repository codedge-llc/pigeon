defmodule Pigeon.NotificationTest do
  use ExUnit.Case

  @reg_id "123456"

  test "GCM new with one registration_id" do
    expected_result = %Pigeon.GCM.Notification{
      registration_id: @reg_id,
      payload: %{},
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(@reg_id) == expected_result
  end

  test "GCM new with multiple registration_ids" do
    reg_ids = ["aaaaaa", "bbbbbb", "cccccc"]

    expected_result = %Pigeon.GCM.Notification{
      registration_id: reg_ids,
      payload: %{},
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(reg_ids) == expected_result
  end

  test "GCM new with notification map" do
    n = %{
      "body" => "test body",
      "title" => "Test Push",
      "icon" => "icon"
    }
    expected_result = %Pigeon.GCM.Notification{
      registration_id: @reg_id,
      payload: %{"notification" => n},
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(@reg_id, n) == expected_result
  end

  test "GCM new with data map" do
    data = %{
      "message" => "test"
    }
    expected_result = %Pigeon.GCM.Notification{
      registration_id: @reg_id,
      payload: %{"data" => data},
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(@reg_id, %{}, data) == expected_result
  end

  test "GCM new with notification and data maps" do
    n = %{
      "body" => "test body",
      "title" => "Test Push",
      "icon" => "icon"
    }
    data = %{
      "message" => "test"
    }
    expected_result = %Pigeon.GCM.Notification{
      registration_id: @reg_id,
      payload: %{"notification" => n, "data" => data},
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(@reg_id, n, data) == expected_result
  end
end
