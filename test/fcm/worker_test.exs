defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case

  alias Pigeon.FCM

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  defp get_consumer_pid(pid) do
    {conn_pid, _ref} =
      pid
      |> :sys.get_state()
      |> Map.get(:consumers)
      |> Map.values()
      |> List.first()

    conn_pid
  end

  defp send_push(pid, count) do
    n = FCM.Notification.new(valid_fcm_reg_id(), %{}, %{"message" => "Test"})

    1..count
    |> Enum.each(fn _x ->
      assert _notif = Pigeon.FCM.push(n, to: pid)
    end)
  end

  defp assert_connection_count(pid, count) do
    assert :sys.get_state(pid).state.connections == count
  end

  test "decrements connection count after disconnect" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]

    {:ok, pid} = FCM.start_connection(opts)

    send_push(pid, 3)

    assert_connection_count(pid, 1)

    conn_pid = get_consumer_pid(pid)

    send(conn_pid, {:closed, self()})

    Process.sleep(500)
    assert_connection_count(pid, 0)

    send_push(pid, 2)

    Process.sleep(500)
    assert_connection_count(pid, 1)
  end

  test "decrements connection count after :down message" do
    opts = [
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]

    {:ok, pid} = FCM.start_connection(opts)

    send_push(pid, 1)

    assert_connection_count(pid, 1)

    conn_pid = get_consumer_pid(pid)
    Process.exit(conn_pid, :kill)

    Process.sleep(500)
    assert_connection_count(pid, 0)

    send_push(pid, 1)

    Process.sleep(500)
    assert_connection_count(pid, 1)
  end
end
