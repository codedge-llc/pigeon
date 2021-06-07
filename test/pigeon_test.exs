defmodule PigeonTest do
  use ExUnit.Case

  @n %Pigeon.FCM.Notification{target: {:token, "test"}}

  test "json_library/0 defaults to Jason" do
    assert Pigeon.json_library() == Jason
  end

  test "default_pool_size/0 defaults to 5" do
    assert Pigeon.default_pool_size() == 5
  end

  describe "push/3" do
    test "synchronously sends a push by default" do
      n = Pigeon.push(PigeonTest.Sandbox, @n)
      assert n.response == :success
    end

    test "synchronously sends a list of notifications" do
      notifs = Pigeon.push(PigeonTest.Sandbox, [@n, @n])
      for n <- notifs, do: assert(n.response == :success)
    end

    test "response: :timeout if timed out" do
      n = Pigeon.push(PigeonTest.Sandbox, @n, timeout: 0)
      assert n.response == :timeout

      notifs = Pigeon.push(PigeonTest.Sandbox, [@n, @n], timeout: 0)
      for n <- notifs, do: assert(n.response == :timeout)
    end

    test "response: :not_started if pid not alive" do
      pid = Process.spawn(& &1, [])
      Process.exit(pid, :stop)
      n = Pigeon.push(pid, @n)
      assert n.response == :not_started
    end
  end

  describe "push/3 with on_response" do
    test "asynchronously sends a push" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      assert Pigeon.push(PigeonTest.Sandbox, @n, on_response: on_response) ==
               :ok

      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
    end

    test "asynchronously sends a list of notifications" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      assert Pigeon.push(PigeonTest.Sandbox, [@n, @n], on_response: on_response) ==
               :ok

      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
      assert_receive(%Pigeon.FCM.Notification{response: :success}, 5_000)
    end

    test "blackholes the response if nil" do
      assert Pigeon.push(PigeonTest.Sandbox, @n, on_response: nil) == :ok
    end

    test "accepts {m, f} tuple" do
      on_response = {PigeonTest, :assert_success}

      assert Pigeon.push(PigeonTest.Sandbox, @n, on_response: on_response) ==
               :ok
    end

    test "accepts {m, f, a} tuple" do
      on_response = {PigeonTest, :assert_success, [:other_data]}

      assert Pigeon.push(PigeonTest.Sandbox, @n, on_response: on_response) ==
               :ok
    end
  end

  def assert_success(notification, _opts \\ []) do
    assert notification.response == :success
  end
end
