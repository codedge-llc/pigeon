defmodule Pigeon.APNS.ConfigTest do
  use ExUnit.Case
  doctest Pigeon.APNS.Config, import: true

  @invalid_cert_msg ~r/^attempted to start without valid certificate/
  @invalid_key_msg ~r/^attempted to start without valid key/

  test "raises if configured with invalid cert raw text" do
    assert_raise(Pigeon.ConfigError, @invalid_cert_msg, fn ->
      config =
        Pigeon.APNS.Config.new(
          cert: "thisisnotacert",
          key: "test/support/test_key.pem-mock"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured with invalid cert filepath" do
    assert_raise(Pigeon.ConfigError, @invalid_cert_msg, fn ->
      config =
        Pigeon.APNS.Config.new(
          cert: "does_not_exist.pem",
          key: "test/support/test_key.pem-mock"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured with invalid key raw text" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config =
        Pigeon.APNS.Config.new(
          cert: "test/support/test_cert.pem-mock",
          key: "thisisnotakey"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end

  test "raises if configured with invalid key filepath" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config =
        Pigeon.APNS.Config.new(
          cert: "test/support/test_cert.pem-mock",
          key: "does_not_exist.pem"
        )

      Pigeon.Worker.init({:ok, config})
    end)
  end
end
