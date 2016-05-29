defmodule Pigeon.NotificationTest do
  use ExUnit.Case

  test "GCM new with one registration_id" do
    reg_id = "123456"
    data = %{key: "value"}

    expected_result = %Pigeon.GCM.Notification{
      registration_id: reg_id,
      data: data,
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(data, reg_id) == expected_result
  end

  test "GCM new with multiple registration_ids" do
    reg_ids = ["aaaaaa", "bbbbbb", "cccccc"]
    data = %{key: "value"}

    expected_result = %Pigeon.GCM.Notification{
      registration_id: reg_ids,
      data: data,
      message_id: nil,
      updated_registration_id: nil
    }
    assert Pigeon.GCM.Notification.new(data, reg_ids) == expected_result
  end
end
