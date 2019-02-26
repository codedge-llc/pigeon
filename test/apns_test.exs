defmodule Pigeon.APNSTest do
  use ExUnit.Case
  doctest Pigeon.APNS
  doctest Pigeon.APNS.Notification

  def test_message(msg) do
    "#{DateTime.to_string(DateTime.utc_now())} - #{msg}"
  end

  def test_topic, do: Application.get_env(:pigeon, :test)[:apns_topic]
  def test_token, do: Application.get_env(:pigeon, :test)[:valid_apns_token]

  def bad_token do
    "00fc13adff785122b4ad28809a3420982341241421348097878e577c991de8f0"
  end

  def bad_id, do: "123e4567-e89b-12d3-a456-42665544000"

  def test_notification(msg) do
    msg
    |> test_message()
    |> Pigeon.APNS.Notification.new(
      test_token(),
      test_topic()
    )
    |> Map.put(:expiration, 0)
    |> Map.put(:collapse_id, "test")
    |> Map.put(:priority, 10)
  end

  describe "start_connection/1" do
    test "starts connection with opts keyword list" do
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev
      ]

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      assert is_pid(pid)

      worker = :sys.get_state(pid)
      assert worker.state.config.uri == "api.development.push.apple.com"
      assert worker.state.config.ping_period == 600_000

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      assert is_pid(pid)
    end

    test "configures ping_period if specified" do
      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev,
        ping_period: 30_000
      ]

      {:ok, pid} = Pigeon.APNS.start_connection(opts)
      worker = :sys.get_state(pid)
      assert worker.state.config.ping_period == 30_000
    end
  end

  describe "push/1" do
    test "returns notification with :success on successful push" do
      n = test_notification("push/1")
      assert Pigeon.APNS.push(n).response == :success
    end

    test "returns notification with response error on unsuccessful push" do
      n =
        Pigeon.APNS.Notification.new(
          test_message("push/1"),
          "bad_token",
          test_topic()
        )

      assert Pigeon.APNS.push(n).response == :bad_device_token
    end

    test "returns list for multiple notifications" do
      n = test_notification("push/1")

      bad_n =
        Pigeon.APNS.Notification.new(
          test_message("push/1"),
          "asdf1234",
          test_topic()
        )

      actual = Pigeon.APNS.push([n, n, bad_n])

      assert Enum.map(actual, & &1.response) == [
               :success,
               :success,
               :bad_device_token
             ]
    end
  end

  describe "push/1 with custom worker" do
    test "pushes to worker pid" do
      n = test_notification("push/1, custom_worker")

      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev
      ]

      {:ok, worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, to: worker_pid).response == :success
    end

    test "pushes to worker's atom name" do
      n = test_notification("push/1, custom_worker")

      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev,
        name: :custom
      ]

      {:ok, _worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, to: :custom).response == :success
    end
  end

  describe "push/2 with :on_response" do
    test "returns :success response on successful push" do
      pid = self()
      on_response = fn x -> send(pid, x) end
      n = test_notification("push/2 :ok")

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive(%Pigeon.APNS.Notification{response: :success}, 5_000)
    end

    test "returns :bad_message_id response if apns-id is invalid" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :bad_message_id"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic(), bad_id())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :bad_message_id},
        5_000
      )
    end

    test "returns :bad_device_token if token is invalid" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :bad_device_token"
        |> test_message()
        |> Pigeon.APNS.Notification.new(bad_token(), test_topic())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :bad_device_token},
        5_000
      )
    end

    test "returns :missing_topic reponse on missing topic for certs supporting mult topics" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :missing_topic"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token())

      assert Pigeon.APNS.push(n, on_response: on_response) == :ok

      assert_receive(%Pigeon.APNS.Notification{response: :missing_topic}, 5_000)
    end
  end

  describe "push/1 with :on_response to custom worker" do
    test "sends to pid if specified" do
      pid = self()
      on_response = fn x -> send(pid, x) end
      n = test_notification("push/2 :ok, custom worker")

      Pigeon.APNS.stop_connection(:default)

      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev
      ]

      {:ok, worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, on_response: on_response, to: worker_pid) ==
               :ok

      assert_receive(%Pigeon.APNS.Notification{response: :success}, 5_000)

      Pigeon.APNS.start_connection(:apns_default)
    end

    test "sends to worker's atom name if specified" do
      pid = self()
      on_response = fn x -> send(pid, x) end
      n = test_notification("push/2 :ok, custom worker")

      opts = [
        cert: Application.get_env(:pigeon, :test)[:apns_cert],
        key: Application.get_env(:pigeon, :test)[:apns_key],
        jwt_key: Application.get_env(:pigeon, :test)[:apns_jwt_key],
        jwt_key_identifier: Application.get_env(:pigeon, :test)[:apns_jwt_key_identifier],
        jwt_team_id: Application.get_env(:pigeon, :test)[:apns_jwt_team_id],
        mode: :dev,
        name: :custom
      ]

      {:ok, _worker_pid} = Pigeon.APNS.start_connection(opts)

      assert Pigeon.APNS.push(n, on_response: on_response, to: :custom) == :ok

      assert_receive(%Pigeon.APNS.Notification{response: :success}, 5_000)
    end
  end
end
