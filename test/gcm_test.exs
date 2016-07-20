defmodule Pigeon.GCMTest do
  use ExUnit.Case

  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  test "successfully sends a valid push" do
    reg_id = Application.get_env(:pigeon, :valid_gcm_reg_id)
    result =
      reg_id
      |> Pigeon.GCM.Notification.new(%{}, @data)
      |> Pigeon.GCM.push

    assert result == :ok
  end

  test "successfully sends a valid push with an explicit config" do
    reg_id = Application.get_env(:pigeon, :valid_gcm_reg_id)
    result =
      reg_id
      |> Pigeon.GCM.Notification.new(%{}, @data)
      |> Pigeon.GCM.push(%{gcm_key: System.get_env("GCM_KEY")})

    assert result == :ok
  end

  test "successfully sends a valid push with callback" do
    reg_id = Application.get_env(:pigeon, :valid_gcm_reg_id)
    n = Pigeon.GCM.Notification.new(reg_id, %{}, @data)

    Pigeon.GCM.push(n, fn(x) -> send self, x end)

    assert_receive {_ref, [{:ok, notification}]}, 5000
    assert notification.registration_id == reg_id
    assert notification.payload == %{"data" => @data}
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_registration_id"
    n = Pigeon.GCM.Notification.new(reg_id, %{}, @data)

    Pigeon.GCM.push(n, fn(x) -> send self, x end)

    assert_receive {_ref, [{:error, :invalid_registration, n}]}, 5000
    assert n.registration_id == reg_id
    assert n.payload == %{"data" => @data}
  end

  test "parse_result with success" do
    assert Pigeon.GCM.parse_result(%{ "message_id" => "1:0408" }) == {:ok, "1:0408"}
  end

  test "parse_result with success and new registration_id" do
    assert Pigeon.GCM.parse_result(%{ "message_id" => "1:2342", "registration_id" => "32" }) ==
      {:ok, "1:2342", "32"}
  end

  test "parse_result with error unavailable" do
    assert Pigeon.GCM.parse_result(%{ "error" => "Unavailable" }) == {:error, :unavailable}
  end

  test "encode_requests with one registration_id" do
    registration_id = [["123456"]]
    assert Pigeon.GCM.encode_requests(registration_id, @payload) ==
      [{"123456", ~S({"to":"123456","data":{"message":"Test push"}})}]
  end

  test "encode_requests with multiple registration_ids" do
    registration_id = ["aaaaaa", "bbbbbb", "cccccc"]
    expected = ~S({"registration_ids":["aaaaaa","bbbbbb","cccccc"],"data":{"message":"Test push"}})
    assert Pigeon.GCM.encode_requests([registration_id], @payload) == [{registration_id, expected}]
  end

  test "encode_requests with over 1000 registration_ids" do
    reg_ids = Enum.chunk(Enum.to_list(1..2500), 1000, 1000, [])
    result = Pigeon.GCM.encode_requests(reg_ids, @payload)
    assert Enum.count(result) == 3
  end
end
