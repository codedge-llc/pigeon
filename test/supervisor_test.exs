defmodule Pigeon.SupervisorTest do
  use ExUnit.Case

  alias Pigeon.Supervisor

  describe "apns_keys?" do
    test "returns true if env :apns_mode, :apns_cert, and :apns_key are set" do
      assert Supervisor.apns_keys?
    end

    test "returns false if not set" do
      mode = Application.get_env(:pigeon, :apns_mode)
      Application.put_env(:pigeon, :apns_mode, nil)

      refute Supervisor.apns_keys?

      Application.put_env(:pigeon, :apns_mode, mode)
    end
  end

  test "valid_apns_config? returns true if proper ssl config keys present" do
    assert Supervisor.valid_apns_config?(Supervisor.ssl_config)
  end

  describe "adm_configured?" do
    test "returns true if env :adm_client_id, and :adm_client_secret are set" do
      assert Supervisor.adm_configured?
    end

    test "returns false if not set" do
      client_id = Application.get_env(:pigeon, :adm_client_id)
      Application.put_env(:pigeon, :adm_client_id, nil)

      refute Supervisor.adm_configured?

      Application.put_env(:pigeon, :adm_client_id, client_id)
    end
  end

  test "valid_adm_config? returns true if proper Amazon ADM config keys present" do
    assert Supervisor.valid_adm_config?(Supervisor.adm_config)
  end
end
