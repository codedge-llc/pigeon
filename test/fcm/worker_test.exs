defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case

  alias Pigeon.FCM

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  test "reconnects on push send after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)
    send(pid, {:closed, self()})

    refute :sys.get_state(pid).socket

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
    expected = [success: valid_fcm_reg_id()]
    assert Pigeon.FCM.push(n, to: pid).response == expected

    assert :sys.get_state(pid).socket
  end

  test "resets stream id after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)

    send(pid, {:closed, self()})
    assert :sys.get_state(pid).stream_id == 7

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)

    assert :sys.get_state(pid).stream_id == 5
  end
end
