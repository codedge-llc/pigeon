defmodule Pigeon.ADMTest do
  use ExUnit.Case
  doctest Pigeon.ADM, import: true
  doctest Pigeon.ADM.Config, import: true

  alias Pigeon.ADM.Notification

  @invalid_key_msg ~r/^attempted to start without valid client id and secret/

  describe "init/1" do
    test "initializes correctly if configured with client id and secret" do
      opts = [client_id: "amzn1.iba-client.abc123", client_secret: "abc123"]

      expected =
        {:ok,
         %{
           config: %Pigeon.ADM.Config{
             client_id: "amzn1.iba-client.abc123",
             client_secret: "abc123"
           },
           access_token: nil,
           access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
           access_token_expiration_seconds: 0,
           access_token_type: nil
         }}

      assert Pigeon.ADM.init(opts) == expected
    end

    test "raises if configured with invalid client id" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        opts = [client_id: nil, client_secret: "abc123"]
        Pigeon.ADM.init(opts)
      end)
    end

    test "raises if configured with invalid client secret" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        opts = [
          client_id: "amzn1.iba-client.abc123",
          client_secret: nil
        ]

        Pigeon.ADM.init(opts)
      end)
    end
  end

  describe "handle_push/3" do
    test "returns an error on pushing with a bad registration_id" do
      reg_id = "bad_reg_id"
      n = Notification.new(reg_id, %{"message" => "example"})
      pid = self()
      PigeonTest.ADM.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{}, 5000)
      assert n.response == :invalid_registration_id
      assert n.registration_id == reg_id
      assert n.payload == %{"data" => %{"message" => "example"}}
    end

    test "handles nil on_response" do
      n = Notification.new("bad_reg_id", %{"message" => "example"})
      PigeonTest.ADM.push(n, on_response: nil)
    end
  end

  test "handle_info/2 handles random messages" do
    assert Pigeon.ADM.handle_info("random", %{}) == {:noreply, %{}}
  end
end
