defmodule Pigeon.LegacyFCMTest do
  use ExUnit.Case
  doctest Pigeon.LegacyFCM, import: true
  doctest Pigeon.LegacyFCM.Config, import: true

  @invalid_key_msg ~r/^attempted to start without valid key/

  describe "init/1" do
    test "raises if configured with invalid key" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        [key: nil]
        |> Pigeon.LegacyFCM.init()
      end)
    end
  end
end
