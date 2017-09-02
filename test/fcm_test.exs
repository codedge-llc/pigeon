defmodule Pigeon.FCMTest do
  use ExUnit.Case
  doctest Pigeon.FCM.Notification, import: true

  alias Pigeon.FCM.{Notification, NotificationResponse}
  require Logger

  @data %{"message" => "Test push"}

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  describe "start_connection/1" do
    test "starts conneciton with opts keyword list" do
      fcm_key = Application.get_env(:pigeon, :test)[:fcm_key]
      opts = [
        key: fcm_key
      ]
      {:ok, pid} = Pigeon.FCM.start_connection(opts)
      assert is_pid(pid)

      state = :sys.get_state(pid)
      assert state.config.key == fcm_key
      assert is_pid(state.socket)
    end

    test "configures ping_period if specified" do
      fcm_key = Application.get_env(:pigeon, :test)[:fcm_key]
      opts = [
        key: fcm_key,
        ping_period: 30_000
      ]

      {:ok, pid} = Pigeon.FCM.start_connection(opts)
      state = :sys.get_state(pid)
      assert state.config.ping_period == 30_000
    end
  end

  describe "push/2 with custom worker" do
    test "pushes to worker pid" do
      n =
        valid_fcm_reg_id()
        |> Notification.new(%{}, @data)

      opts = [
        key: Application.get_env(:pigeon, :test)[:fcm_key]
      ]
      {:ok, worker_pid} = Pigeon.FCM.start_connection(opts)

      assert {:ok, _notif} = Pigeon.FCM.push(n, to: worker_pid)
    end

    test "pushes to worker's atom name" do
      n =
        valid_fcm_reg_id()
        |> Notification.new(%{}, @data)

      opts = [
        key: Application.get_env(:pigeon, :test)[:fcm_key],
        name: :custom
      ]
      {:ok, _worker_pid} = Pigeon.FCM.start_connection(opts)

      assert {:ok, _notif} = Pigeon.FCM.push(n, to: :custom)
    end
  end

  test "successfully sends a valid push" do
    {:ok, notification} =
      valid_fcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.FCM.push

    assert notification.ok == [valid_fcm_reg_id()]
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
      error: %{"error" => ["2"], "error2" => ["1"]}
    }
    assert Pigeon.FCM.merge(nr1, nr2) ==
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
    assert [{^regs, _encoded}] = Pigeon.FCM.encode_requests(notification)
  end

  test "Message for over 1000 recipients should be chunked" do
    regs = Enum.to_list(1..2534)
    notification = Notification.new(regs, %{}, @data)
    res = Pigeon.FCM.encode_requests(notification)
    assert [{r1, _e1}, {r2, _e2}, {r3, _e3}]  = res
    assert length(r1) == 1000
    assert length(r2) == 1000
    assert length(r3) == 534
  end

  test "successfully sends a valid push with an explicit key" do
    response =
      valid_fcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.FCM.push(key: "explicit")

     assert response == {:error, :unauthorized}
  end

  test "successfully sends a valid push with callback" do
    reg_id = valid_fcm_reg_id()
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.FCM.push(n, on_response: fn(x) -> send pid, x end)

    assert_receive {:ok, notification}, 5000
    assert notification.ok == [reg_id]
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_reg_id"
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.FCM.push(n, on_response: fn(x) -> send pid, x end)

    assert_receive {:ok, %NotificationResponse{remove: [^reg_id]}}, 5000
    assert n.registration_id == reg_id
    assert n.payload == %{"data" => @data}
  end

  test "encode_requests with one registration_id" do
    registration_id = "123456"
    payload = Notification.new(registration_id, %{},@data)
    assert Pigeon.FCM.encode_requests(payload) ==
      [{["123456"], ~S({"to":"123456","priority":"normal","data":{"message":"Test push"}})}]
  end

  test "encode_requests with multiple registration_ids" do
    registration_id = ["a", "b", "c"]
    payload = Notification.new(registration_id, %{}, %{"k" => "v"})
    expected = ~S({"registration_ids":["a","b","c"],"priority":"normal","data":{"k":"v"}})
    assert Pigeon.FCM.encode_requests(payload) == [{registration_id, expected}]
  end
end
