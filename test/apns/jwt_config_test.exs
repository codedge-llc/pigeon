defmodule Pigeon.APNS.JWTConfigTest do
  use ExUnit.Case
  doctest Pigeon.APNS.JWTConfig, import: true

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
end
