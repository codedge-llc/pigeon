defmodule PigeonTest do
  use ExUnit.Case

  @notif %Pigeon.FCM.Notification{target: {:token, "test"}}

  test "json_library/0 defaults to Jason" do
    assert Pigeon.json_library() == Jason
  end

  describe "push/3" do
    test "synchronously sends a push by default" do
      n = Pigeon.push(PigeonTest.Sandbox, @notif)
      assert n.response == :success
    end

    test "synchronously sends a list of notifications" do
      notifs = Pigeon.push(PigeonTest.Sandbox, [@notif, @notif])
      for n <- notifs, do: assert(n.response == :success)
    end

    test "response: :timeout if timed out" do
      n = Pigeon.push(PigeonTest.Sandbox, @notif, timeout: 0)
      assert n.response == :timeout

      notifs = Pigeon.push(PigeonTest.Sandbox, [@notif, @notif], timeout: 0)
      for n <- notifs, do: assert(n.response == :timeout)
    end

    test "response: :not_started if pid not alive" do
      pid = Process.spawn(& &1, [])
      Process.exit(pid, :stop)
      n = Pigeon.push(pid, @notif)
      assert n.response == :not_started
    end
  end

  describe "push/3 with on_response" do
    test "asynchronously sends a push" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      assert Pigeon.push(PigeonTest.Sandbox, @notif, on_response: on_response) == :ok
      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
    end

    test "asynchronously sends a list of notifications" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      assert Pigeon.push(PigeonTest.Sandbox, [@notif, @notif], on_response: on_response) ==
               :ok

      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
    end

    test "blackholes the response if nil" do
      assert Pigeon.push(PigeonTest.Sandbox, @notif, on_response: nil) == :ok
    end
  end
end
