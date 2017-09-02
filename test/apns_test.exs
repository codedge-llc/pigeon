defmodule Pigeon.APNSTest do
  use ExUnit.Case
  doctest Pigeon.APNS.Notification

  def test_message(msg), do: "#{DateTime.to_string(DateTime.utc_now())} - #{msg}"
  def test_topic, do: Application.get_env(:pigeon, :test)[:apns_topic]
  def test_token, do: Application.get_env(:pigeon, :test)[:valid_apns_token]
  def bad_token, do: "00fc13adff785122b4ad28809a3420982341241421348097878e577c991de8f0"
  def bad_id, do: "123e4567-e89b-12d3-a456-42665544000"

  describe "start_connection/1" do
    test "starts connection with opts keyword list" do
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev
      ]

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      assert is_pid(pid)

      state = :sys.get_state(pid)
      assert state.config.mode == :dev
      assert state.config.ping_period == 600_000

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      assert is_pid(pid)
    end

    test "configures ping_period if specified" do
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev,
        ping_period: 30_000
      ]

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      state = :sys.get_state(pid)
      assert state.config.ping_period == 30_000
    end
  end

  describe "push/1" do
    test "returns {:ok, notification} on successful push" do
      n = Pigeon.APNS.Notification.new(test_message("push/1"), test_token(), test_topic())
      assert {:ok, _notif} = Pigeon.APNS.push(n)
    end

    test "returns {:error, reason, notification} on unsuccessful push" do
      n = Pigeon.APNS.Notification.new(test_message("push/1"), "bad_token", test_topic())
      assert {:error, :bad_device_token, _notif} = Pigeon.APNS.push(n)
    end

    test "returns list of response tuples for multiple notifications" do
      n = Pigeon.APNS.Notification.new(test_message("push/1"), test_token(), test_topic())
      bad_n = Pigeon.APNS.Notification.new(test_message("push/1"), "asdf1234", test_topic())
      assert %{
        ok: [_n1, _n2],
        error: %{
          bad_device_token: [_n3]
        }
      } = Pigeon.APNS.push([n, n, bad_n])
    end
  end

  describe "push/1 with custom worker" do
    test "pushes to worker pid" do
      n =
        Pigeon.APNS.Notification.new(
          test_message("push/1, custom worker"),
          test_token(), test_topic()
        )

      Pigeon.APNS.stop_connection(:default)
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev
      ]
      {:ok, worker_pid} = Pigeon.APNS.start_connection(opts)

      assert {:ok, _notif} = Pigeon.APNS.push(n, to: worker_pid)

      Pigeon.APNS.start_connection(:apns_default)
    end

    test "pushes to worker's atom name" do
      n =
        Pigeon.APNS.Notification.new(
          test_message("push/1, custom worker"),
          test_token(), test_topic()
        )

      Pigeon.APNS.stop_connection(:default)
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev,
        name: :custom
      ]
      {:ok, _worker_pid} = Pigeon.APNS.start_connection(opts)

      assert {:ok, _notif} = Pigeon.APNS.push(n, to: :custom)

      Pigeon.APNS.start_connection(:apns_default)
    end
  end

  describe "push/1 with :on_response" do
    test "returns {:ok, notification} on successful push" do
      pid = self()
      on_response = fn(x) -> send pid, x end

      n =
        "push/2 :ok"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive({:ok, _notif}, 5_000)
    end

    test "returns {:error, :bad_message_id, n} if apns-id is invalid" do
      pid = self()
      on_response = fn(x) -> send pid, x end
      n =
        "push/2 :bad_message_id"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic(), bad_id())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive({:error, :bad_message_id, _n}, 5_000)
    end

    test "returns {:error, :bad_device_token, n} if token is invalid" do
      pid = self()
      on_response = fn(x) -> send pid, x end
      n =
        "push/2 :bad_device_token"
        |> test_message()
        |> Pigeon.APNS.Notification.new(bad_token(), test_topic())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive({:error, :bad_device_token, _n}, 5_000)
    end

    test "returns {:error, :missing_topic, n} on missing topic for certs supporting mult topics" do
      pid = self()
      on_response = fn(x) -> send pid, x end
      n =
        "push/2 :missing_topic"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive({:error, :missing_topic, _n}, 5_000)
    end
  end

  describe "push/1 with :on_response to custom worker" do
    test "sends to pid if specified" do
      pid = self()
      on_response = fn(x) -> send pid, x end

      n =
        "push/2 :ok, custom worker"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic())

      Pigeon.APNS.stop_connection(:default)
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev
      ]
      {:ok, worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, on_response: on_response, to: worker_pid) == :ok

      assert_receive({:ok, _notif}, 5_000)

      Pigeon.APNS.start_connection(:apns_default)
    end

    test "sends to worker's atom name if specified" do
      pid = self()
      on_response = fn(x) -> send pid, x end

      n =
        "push/2 :ok, custom worker"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic())

      Pigeon.APNS.stop_connection(:default)
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        mode: :dev,
        name: :custom
      ]
      {:ok, _worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, on_response: on_response, to: :custom) == :ok

      assert_receive({:ok, _notif}, 5_000)

      Pigeon.APNS.start_connection(:apns_default)
    end
  end
end
