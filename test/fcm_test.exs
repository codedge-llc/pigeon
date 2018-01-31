defmodule Pigeon.FCMTest do
  use ExUnit.Case
  doctest Pigeon.FCM, import: true

  alias Pigeon.FCM
  alias Pigeon.FCM.Notification
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

      worker = :sys.get_state(pid)
      assert worker.state.config.key == fcm_key
      assert worker.state.connections == 0
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

      expected = [success: valid_fcm_reg_id()]
      actual = Pigeon.FCM.push(n, to: worker_pid).response
      assert actual == expected
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

      expected = [success: valid_fcm_reg_id()]
      actual = Pigeon.FCM.push(n, to: :custom).response
      assert actual == expected
    end
  end

  test "successfully sends a valid push" do
    notification =
      valid_fcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.FCM.push()

    expected = [success: valid_fcm_reg_id()]
    assert notification.response == expected
  end

  test "successfully sends a valid push with an explicit key" do
    notif =
      valid_fcm_reg_id()
      |> Notification.new(%{}, @data)
      |> Pigeon.FCM.push(key: "explicit")

    assert notif.status == :unauthorized
  end

  test "successfully sends a valid push with callback" do
    reg_id = valid_fcm_reg_id()
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    FCM.push(n, on_response: fn x -> send(pid, x) end)

    assert_receive(n = %Notification{response: regids}, 5000)
    assert n.status == :success
    assert regids == [success: reg_id]
  end

  test "returns an error on pushing with a bad registration_id" do
    reg_id = "bad_reg_id"
    n = Notification.new(reg_id, %{}, @data)
    pid = self()
    Pigeon.FCM.push(n, on_response: fn x -> send(pid, x) end)

    assert_receive(n = %Notification{}, 5000)
    assert n.status == :success
    assert n.response == [invalid_registration: reg_id]
    assert n.registration_id == reg_id
    assert n.payload == %{"data" => @data}
  end
end
