defmodule Pigeon.GCMTest do
  use ExUnit.Case

  test "successfully sends a valid push" do
    reg_id = Application.get_env(:pigeon, :valid_gcm_reg_id)
    data = %{message: "Test push"}
    result = Pigeon.GCM.Notification.new(data, reg_id)
      |> Pigeon.GCM.push

    assert result == :ok
  end

  test "successfully sends a valid push with callback" do
    reg_id = Application.get_env(:pigeon, :valid_gcm_reg_id)
    data = %{message: "Test push"}
    n = Pigeon.GCM.Notification.new(data, reg_id)

    Pigeon.GCM.push(n, fn(x) -> send self, x end)

    assert_receive {_ref, [{:ok, notification}]}, 5000
    assert notification.registration_id == reg_id
    assert notification.data == data
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_registration_id"
    data = %{message: "Test push"}
    n = Pigeon.GCM.Notification.new(data, reg_id)

    Pigeon.GCM.push(n, fn(x) -> send self, x end)

    assert_receive {_ref, [{:error, :InvalidRegistration, n}]}, 5000
    assert n.registration_id == reg_id
    assert n.data == data
  end

  test "parse_result with success" do
    assert Pigeon.GCM.parse_result(%{ "message_id" => "1:0408" }) == {:ok, "1:0408"}
  end

  test "parse_result with success and new registration_id" do
    assert Pigeon.GCM.parse_result(%{ "message_id" => "1:2342", "registration_id" => "32" }) == {:ok, "1:2342", "32"}
  end

  test "parse_result with error unavailable" do
    assert Pigeon.GCM.parse_result(%{ "error" => "Unavailable" }) == {:error, :Unavailable}
  end

  test "encode_requests with one registration_id" do
    data = %{key: "value"}
    registration_id = [["123456"]]
    assert Pigeon.GCM.encode_requests(registration_id, data) == [{"123456", "{\"to\":\"123456\",\"data\":{\"key\":\"value\"}}"}]
  end

  test "encode_requests with multiple registration_ids" do
    data = %{key: "value"}
    registration_id = ["aaaaaa", "bbbbbb", "cccccc"]
    assert Pigeon.GCM.encode_requests([registration_id], data) == [{registration_id, "{\"registration_ids\":[\"aaaaaa\",\"bbbbbb\",\"cccccc\"],\"data\":{\"key\":\"value\"}}"}]
  end

  test "encode_requests with over 1000 registration_ids" do
    data = %{key: "value"}
    reg_ids = Enum.chunk(Enum.to_list(1..2500), 1000, 1000, [])
    result = Pigeon.GCM.encode_requests(reg_ids, data)
    assert Enum.count(result) == 3
  end
end
