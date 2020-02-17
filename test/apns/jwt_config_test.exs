defmodule Pigeon.APNS.JWTConfigTest do
  use ExUnit.Case
  doctest Pigeon.APNS.JWTConfig, import: true

  @invalid_team_id_msg ~r/^attempted to start without valid team_id/
  @invalid_key_id_msg ~r/^attempted to start without valid key_identifier/
  @invalid_key_msg ~r/^attempted to start without valid key/

  def test_message(msg),
    do: "#{DateTime.to_string(DateTime.utc_now())} - #{msg}"

  def test_topic, do: Application.get_env(:pigeon, :test)[:apns_topic]
  def test_token, do: Application.get_env(:pigeon, :test)[:valid_apns_token]

  test "sends a push with valid config" do
    n =
      Pigeon.APNS.Notification.new(
        test_message("push/1 with jwt"),
        test_token(),
        test_topic()
      )

    assert Pigeon.APNS.push(n, to: :apns_jwt_dynamic).response == :success
  end

  test "raises if configured with invalid key raw text" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config =
        Pigeon.APNS.JWTConfig.new(
          team_id: "ASDF1234",
          key_identifier: "ASDF1234",
          key: "notvalidkey"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured with invalid key file" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config =
        Pigeon.APNS.JWTConfig.new(
          team_id: "ASDF1234",
          key_identifier: "ASDF1234",
          key: "does_not_exist.p8"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured without team_id" do
    assert_raise(Pigeon.ConfigError, @invalid_team_id_msg, fn ->
      config =
        Pigeon.APNS.JWTConfig.new(
          team_id: nil,
          key_identifier: "ASDF1234",
          key: "test/support/AuthKey.p8-mock"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured without key_identifier" do
    assert_raise(Pigeon.ConfigError, @invalid_key_id_msg, fn ->
      config =
        Pigeon.APNS.JWTConfig.new(
          team_id: "ASDF1234",
          key_identifier: nil,
          key: "test/support/AuthKey.p8-mock"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end
end
