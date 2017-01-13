defmodule Pigeon.GCMTest do
  use ExUnit.Case
  alias Pigeon.GCM.Notification
  require Logger
  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  defp valid_gcm_reg_id, do: Application.get_env(:pigeon, :test)[:valid_gcm_reg_id]

  test "successfully sends a valid push" do
    {:ok, [result]} =
      valid_gcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.GCM.push

    assert elem(result, 0) == :ok
    assert elem(result, 2) == valid_gcm_reg_id()
  end

  #test "successfully sends a valid push with an explicit config" do
  #  {:ok, [result]} =
  #    valid_gcm_reg_id()
  #    |> Notification.new(%{}, @data)
  #    |> Pigeon.GCM.push(%{gcm_key: "explicit"})
#
  #    assert elem(result, 0) == :ok
  #    assert elem(result, 2) == valid_gcm_reg_id()
  #end

  test "successfully sends a valid push with callback" do
    reg_id = valid_gcm_reg_id()
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.GCM.push(n, fn(x) -> send pid, x end, %{})

    assert_receive {:ok, [{:ok, id, reg_id}]}, 5000
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_registration_id"
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.GCM.push(n, fn(x) -> send pid, x end, %{})

    assert_receive {:ok, [{:remove, reg_id}]}, 5000
    assert n.registration_id == reg_id
    assert n.payload == %{"data" => @data}
  end

  

  test "encode_requests with one registration_id" do
    registration_id = "123456"
    payload = Notification.new(registration_id, %{},@data)
    assert Pigeon.GCM.encode_requests(payload) ==
      {"123456", ~S({"to":"123456","data":{"message":"Test push"}})}
  end

  test "encode_requests with multiple registration_ids" do
    registration_id = ["aaaaaa", "bbbbbb", "cccccc"]
    payload = Notification.new(registration_id, %{},@data)
    expected = ~S({"registration_ids":["aaaaaa","bbbbbb","cccccc"],"data":{"message":"Test push"}})
    assert Pigeon.GCM.encode_requests(payload) == {registration_id, expected}
  end

  #test "encode_requests with over 1000 registration_ids" do
  #  reg_ids = Enum.chunk(Enum.to_list(1..2500), 1000, 1000, [])
  #  result = Pigeon.GCM.encode_requests(reg_ids, @payload)
  #  assert Enum.count(result) == 3
  #end
end
