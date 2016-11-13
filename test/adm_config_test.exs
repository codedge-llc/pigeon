defmodule Pigeon.ADM.ConfigTest do
  use ExUnit.Case

  alias Pigeon.ADM.Config

  describe "configured?" do
    test "returns true if env {:adm, :client_id}, and {:adm, :client_secret} are set" do
      assert Config.configured?
    end

    test "returns false if not set" do
      config = Application.get_env(:pigeon, :adm)
      Application.put_env(:pigeon, :adm, List.keystore(config, :client_id, 0, {:client_id, nil}))

      refute Config.configured?

      Application.put_env(:pigeon, :adm, config)
    end
  end

  test "valid_adm_config? returns true if proper Amazon ADM config keys present" do
    assert Config.valid?(Config.default_config)
  end
end
