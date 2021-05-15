defmodule Pigeon.ADM.NotificationTest do
  use ExUnit.Case
  doctest Pigeon.ADM.Notification

  def test_registration_id, do: "test1234"
  def test_data, do: %{message: "your message"}

  test "new/1" do
    expected_result = %Pigeon.ADM.Notification{
      registration_id: test_registration_id(),
      payload: %{"data" => %{}},
      updated_registration_id: nil,
      consolidation_key: nil,
      expires_after: 604_800,
      md5: "1B2M2Y8AsgTpgAmY7PhCfg=="
    }

    assert expected_result ==
             Pigeon.ADM.Notification.new(test_registration_id())
  end

  test "new/2" do
    expected_result = %Pigeon.ADM.Notification{
      registration_id: test_registration_id(),
      payload: %{"data" => %{"message" => "your message"}},
      updated_registration_id: nil,
      consolidation_key: nil,
      expires_after: 604_800,
      md5: "qzF+HgArKZjJrpfcTbiFxg=="
    }

    assert expected_result ==
             Pigeon.ADM.Notification.new(test_registration_id(), test_data())
  end

  describe "calculate_md5/1" do
    test "puts md5 hash if a valid data payload" do
      n = %Pigeon.ADM.Notification{
        payload: %{"data" => %{message: "your message", hi: "bye"}}
      }

      result_n = Pigeon.ADM.Notification.calculate_md5(n)
      assert "w2qyl/pbK7HVl9zfzu7Nww==" == result_n.md5
    end

    test "does nothing if invalid data payload" do
      n = %Pigeon.ADM.Notification{
        payload: :bad
      }

      result_n = Pigeon.ADM.Notification.calculate_md5(n)
      refute result_n.md5
    end
  end

  test "ensure_strings" do
    data = %{
      :message => "your message",
      "string_key" => "string_value",
      "something" => 123,
      456 => true
    }

    n = Pigeon.ADM.Notification.new(test_registration_id(), data)

    expected_result = %{
      "message" => "your message",
      "string_key" => "string_value",
      "something" => "123",
      "456" => "true"
    }

    assert expected_result == n.payload["data"]
  end
end
