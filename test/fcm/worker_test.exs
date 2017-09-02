defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case

  alias Pigeon.FCM

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  test "send malformed JSON" do
    opts = [
      name: :gonecrashing,
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)

    me = self()
    bad = {"toto", "this is not json"}
    :gen_server.cast(pid, {:push, bad, on_response: &(send me, &1)})
    assert_receive {:error, :malformed_json}, 5000

    :gen_server.cast(pid, :stop)
  end

  test "reconnects on push send after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)
    send(pid, {:closed, self()})

    refute :sys.get_state(pid).socket

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test push"})
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)

    assert :sys.get_state(pid).socket
  end

  test "resets stream id after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test push"})
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)

    send(pid, {:closed, self()})
    assert :sys.get_state(pid).stream_id == 7

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test push"})
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)
    assert {:ok, _notif} = Pigeon.FCM.push(n, to: pid)

    assert :sys.get_state(pid).stream_id == 5
  end
end
