defmodule Pigeon.ADM.ConfigTest do
  use ExUnit.Case
  doctest Pigeon.ADM.Config, import: true

  @invalid_key_msg ~r/^attempted to start without valid client id and secret/

  test "success if configured with client id and secret" do
      config = Pigeon.ADM.Config.new(name: :test, client_id: "amzn1.iba-client.abc123", client_secret: "abc123")
      {:ok, _} = Pigeon.ADM.Worker.init({:ok, config})
  end

  test "raises if configured with invalid client id" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config = Pigeon.ADM.Config.new(name: :test, client_id: nil, client_secret: "abc123")
      Pigeon.ADM.Worker.init({:ok, config})
    end)
  end

  test "raises if configured with invalid client secret" do
    assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
      config = Pigeon.ADM.Config.new(name: :test, client_id: "amzn1.iba-client.abc123", client_secret: nil)
      Pigeon.ADM.Worker.init({:ok, config})
    end)
  end
end
