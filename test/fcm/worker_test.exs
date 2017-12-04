defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case

  alias Pigeon.FCM

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  # test "starts new connection on push send if none available" do
  #   opts = [
  #     key: Application.get_env(:pigeon, :test)[:fcm_key]
  #   ]
  #   {:ok, pid} = FCM.start_connection(opts)
  #   send(pid, {:closed, self()})

  #   refute :sys.get_state(pid).socket

  #   n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
  #   expected = [success: valid_fcm_reg_id()]
  #   assert Pigeon.FCM.push(n, to: pid).response == expected

  #   assert :sys.get_state(pid).state.socket
  # end

  test "decrements connection count after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)

    {conn_pid, _ref} =
      pid
      |> :sys.get_state()
      |> Map.get(:consumers)
      |> Map.values
      |> List.first

    assert :sys.get_state(pid).state.connections == 1

    send(conn_pid, {:closed, self()})

    Process.sleep(500)
    assert :sys.get_state(pid).state.connections == 0

    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})
    assert _notif = Pigeon.FCM.push(n, to: pid)
    assert _notif = Pigeon.FCM.push(n, to: pid)

    Process.sleep(500)
    assert :sys.get_state(pid).state.connections == 1
  end
end
