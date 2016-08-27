defmodule Pigeon.APNS.ConfigTest do
  use ExUnit.Case

  alias Pigeon.APNS.Config

  describe "default_keys?" do
    test "returns true if env :apns_mode, :apns_cert, and :apns_key are set" do
      assert Config.default_keys?
    end

    test "returns false if not set" do
      mode = Application.get_env(:pigeon, :apns_mode)
      Application.put_env(:pigeon, :apns_mode, nil)

      refute Config.default_keys?

      Application.put_env(:pigeon, :apns_mode, mode)
    end
  end

  test "valid? returns true if proper ssl config keys present" do
    assert Config.valid?(Config.default_config)
  end
end
