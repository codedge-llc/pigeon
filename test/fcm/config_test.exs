defmodule Pigeon.FCM.ConfigTest do
  use ExUnit.Case
  doctest Pigeon.FCM.Config, import: true

  @invalid_key_msg ~r/^attempted to start without valid key/

  test "raises if configured with invalid key" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config = Pigeon.FCM.Config.new(key: nil)
      Pigeon.Worker.init({:ok, config})
    end)
  end
end
