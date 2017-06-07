defmodule Pigeon.GCMTest do
  use ExUnit.Case
  doctest Pigeon.GCM.Notification

  alias Pigeon.GCM.Notification
  alias Pigeon.GCM.NotificationResponse
  require Logger

  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  defp valid_gcm_reg_id, do: Application.get_env(:pigeon, :test)[:valid_gcm_reg_id]

  test "successfully sends a valid push" do
    {:ok, notification} =
      valid_gcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.GCM.push

    assert notification.ok == [valid_gcm_reg_id()]
  end

  test "Can merge two responses" do
    nr1 = %NotificationResponse{
      ok: ["42"],
      retry: ["12"],
      error: %{"error" => ["1"]}
    }
    nr2 = %NotificationResponse{
      ok: ["43"],
      update: ["12"],
      error: %{"error" => ["2"], "error2"=> ["1"]}
    }
    assert Pigeon.GCM.merge(nr1, nr2) ==
      %NotificationResponse{
        ok: ["42", "43"],
        retry: ["12"],
        update: ["12"],
        error: %{"error" => ["1", "2"] , "error2" => ["1"]}
      }
  end

  test "Message for less than 1000 recipients should not be chunked" do
    regs = Enum.to_list(1..999)
    notification = Notification.new(regs, %{}, @data)
    assert [{^regs, encoded}] = res = Pigeon.GCM.encode_requests(notification)
  end

  test "Message for over 1000 recipients should be chunked" do
    regs = Enum.to_list(1..2534)
    notification = Notification.new(regs, %{}, @data)
    res = Pigeon.GCM.encode_requests(notification)
    assert [{r1, e1}, {r2, e2}, {r3, e3}]  = res
    assert length(r1) == 1000
    assert length(r2) == 1000
    assert length(r3) == 534
  end

  test "successfully sends a valid push with an explicit config" do
    response =
      valid_gcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.GCM.push(%{gcm_key: "explicit"})

     assert response == {:error, :unauthorized}
  end

  test "successfully sends a valid push with callback" do
    reg_id = valid_gcm_reg_id()
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.GCM.send_push(n, fn(x) -> send pid, x end, %{})

    assert_receive {:ok, notification}, 5000
    assert notification.ok == [reg_id]
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_registration_id"
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.GCM.send_push(n, fn(x) -> send pid, x end, %{})

    assert_receive {:ok, %Pigeon.GCM.NotificationResponse{remove: ["bad_registration_id"]}}, 5000
    assert n.registration_id == reg_id
    assert n.payload == %{"data" => @data}
  end

  test "encode_requests with one registration_id" do
    registration_id = "123456"
    payload = Notification.new(registration_id, %{},@data)
    assert Pigeon.GCM.encode_requests(payload) ==
      [{["123456"], ~S({"to":"123456","data":{"message":"Test push"}})}]
  end

  test "encode_requests with multiple registration_ids" do
    registration_id = ["aaaaaa", "bbbbbb", "cccccc"]
    payload = Notification.new(registration_id, %{},@data)
    expected = ~S({"registration_ids":["aaaaaa","bbbbbb","cccccc"],"data":{"message":"Test push"}})
    assert Pigeon.GCM.encode_requests(payload) == [{registration_id, expected}]
  end

end
