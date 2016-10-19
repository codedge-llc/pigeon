defmodule Pigeon.ADMNotificationTest do
  use ExUnit.Case

  def test_registration_id, do: "test1234"
  def test_data, do: %{ message: "your message" }

  test "new" do
    expected_result = %Pigeon.ADM.Notification{
      registration_id: test_registration_id,
      payload: %{"data" => test_data},
      updated_registration_id: nil,
      consolidation_key: nil,
      expires_after: 604800,
      md5: "qzF+HgArKZjJrpfcTbiFxg=="
    }
    assert expected_result == Pigeon.ADM.Notification.new(test_registration_id, test_data)
  end

  test "calculate_md5" do
    n = %Pigeon.ADM.Notification{
      payload: %{"data" => %{ message: "your message", hi: "bye" }}
    }
    result_n = Pigeon.ADM.Notification.calculate_md5(n)
    assert "w2qyl/pbK7HVl9zfzu7Nww==" == result_n.md5
  end
end
