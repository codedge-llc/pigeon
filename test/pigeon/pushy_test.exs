defmodule Pigeon.PushyTest do
  use ExUnit.Case
  doctest Pigeon.Pushy, import: true
  doctest Pigeon.Pushy.Config, import: true

  alias Pigeon.Pushy.Notification

  @invalid_key_msg ~r/^attempted to start without valid key or uri/

  describe "init/1" do
    test "initializes correctly if configured with key" do
      opts = [uri: "api.pushy.me", key: "abc123"]

      expected =
        {:ok,
         %{
           config: %Pigeon.Pushy.Config{
             uri: "api.pushy.me",
             key: "abc123"
           }
         }}

      assert Pigeon.Pushy.init(opts) == expected
    end

    test "raises if configured with invalid uri" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        opts = [uri: nil, key: "abc123"]
        Pigeon.Pushy.init(opts)
      end)
    end

    test "raises if configured with invalid key" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        opts = [
          uri: "api.pushy.me",
          key: nil
        ]

        Pigeon.Pushy.init(opts)
      end)
    end
  end

  describe "handle_push/3" do
    test "returns an error on pushing with a bad registration_id" do
      token = "bad_token"
      n = Notification.new(token, %{"message" => "example"})
      pid = self()
      PigeonTest.Pushy.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{}, 5000)
      assert n.response == :invalid_registration_id
      assert n.registration_id == reg_id
      assert n.payload == %{"data" => %{"message" => "example"}}
    end

    test "handles nil on_response" do
      n = Notification.new("bad_token", %{"message" => "example"})
      PigeonTest.Pushy.push(n, on_response: nil)
    end
  end

  test "handle_info/2 handles random messages" do
    assert Pigeon.ADM.handle_info("random", %{}) == {:noreply, %{}}
  end
end
