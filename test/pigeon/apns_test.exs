defmodule Pigeon.APNSTest do
  use ExUnit.Case
  doctest Pigeon.APNS
  doctest Pigeon.APNS.Config, import: true
  doctest Pigeon.APNS.JWTConfig, import: true
  doctest Pigeon.APNS.Notification

  @invalid_cert_msg ~r/^attempted to start without valid certificate/
  @invalid_key_msg ~r/^attempted to start without valid key/
  @invalid_team_id_msg ~r/^attempted to start without valid team_id/
  @invalid_key_id_msg ~r/^attempted to start without valid key_identifier/
  @default_timeout 10_000

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

  describe "init/1" do
    test "raises if configured with invalid cert raw text" do
      assert_raise(Pigeon.ConfigError, @invalid_cert_msg, fn ->
        [
          cert: "thisisnotacert",
          key: File.read!("test/support/FakeAPNSKey.pem")
        ]
        |> Pigeon.APNS.init()
      end)
    end

    test "raises if configured with invalid key raw text" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        [
          cert: File.read!("test/support/FakeAPNSCert.pem"),
          key: "thisisnotakey"
        ]
        |> Pigeon.APNS.init()
      end)
    end
  end

  describe "init/1 (JWT)" do
    test "sends a push with valid config" do
      n =
        Pigeon.APNS.Notification.new(
          test_message("push/1 with jwt"),
          test_token(),
          test_topic()
        )

      assert PigeonTest.APNS.JWT.push(n).response == :success
    end

    test "raises if configured with invalid key raw text" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        [
          adapter: Pigeon.APNS,
          team_id: "ASDF1234",
          key_identifier: "ASDF1234",
          key: "notvalidkey"
        ]
        |> Pigeon.APNS.init()
      end)
    end

    test "raises if configured with invalid key file" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        [
          adapter: Pigeon.APNS,
          team_id: "ASDF1234",
          key_identifier: "ASDF1234",
          key: "does_not_exist.p8"
        ]
        |> Pigeon.APNS.init()
      end)
    end

    test "raises if configured without team_id" do
      assert_raise(Pigeon.ConfigError, @invalid_team_id_msg, fn ->
        [
          adapter: Pigeon.APNS,
          team_id: nil,
          key_identifier: "ASDF1234",
          key: "test/support/AuthKey.p8-mock"
        ]
        |> Pigeon.APNS.init()
      end)
    end

    test "raises if configured without key_identifier" do
      assert_raise(Pigeon.ConfigError, @invalid_key_id_msg, fn ->
        [
          adapter: Pigeon.APNS,
          team_id: "ASDF1234",
          key_identifier: nil,
          key: "test/support/AuthKey.p8-mock"
        ]
        |> Pigeon.APNS.init()
      end)
    end
  end

  describe "push/1" do
    test "returns notification with :success on successful push" do
      n = test_notification("push/1")
      assert PigeonTest.APNS.push(n).response == :success
    end

    test "returns notification with response error on unsuccessful push" do
      n =
        Pigeon.APNS.Notification.new(
          test_message("push/1"),
          "bad_token",
          test_topic()
        )

      assert PigeonTest.APNS.push(n).response == :bad_device_token
    end

    test "returns list for multiple notifications" do
      n = test_notification("push/1")

      bad_n =
        Pigeon.APNS.Notification.new(
          test_message("push/1"),
          "asdf1234",
          test_topic()
        )

      actual = PigeonTest.APNS.push([n, n, bad_n])

      assert Enum.map(actual, & &1.response) == [
               :success,
               :success,
               :bad_device_token
             ]
    end
  end

  describe "push/2 with :on_response" do
    test "returns :success response on successful push" do
      pid = self()
      on_response = fn x -> send(pid, x) end
      n = test_notification("push/2 :ok")

      assert PigeonTest.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :success},
        @default_timeout
      )
    end

    test "returns :bad_message_id response if apns-id is invalid" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :bad_message_id"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token(), test_topic(), bad_id())

      assert PigeonTest.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :bad_message_id},
        @default_timeout
      )
    end

    test "returns :bad_device_token if token is invalid" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :bad_device_token"
        |> test_message()
        |> Pigeon.APNS.Notification.new(bad_token(), test_topic())

      assert PigeonTest.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :bad_device_token},
        @default_timeout
      )
    end

    test "returns :missing_topic response on missing topic for certs supporting mult topics" do
      pid = self()
      on_response = fn x -> send(pid, x) end

      n =
        "push/2 :missing_topic"
        |> test_message()
        |> Pigeon.APNS.Notification.new(test_token())

      assert PigeonTest.APNS.push(n, on_response: on_response) == :ok

      assert_receive(
        %Pigeon.APNS.Notification{response: :missing_topic},
        @default_timeout
      )
    end
  end
end
