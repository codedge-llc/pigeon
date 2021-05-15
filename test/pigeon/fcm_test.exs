defmodule Pigeon.FCMTest do
  use ExUnit.Case
  doctest Pigeon.FCM, import: true
  doctest Pigeon.FCM.Config, import: true
  doctest Pigeon.FCM.Notification, import: true

  alias Pigeon.FCM.Notification
  require Logger

  @data %{"message" => "Test push"}
  @invalid_key_msg ~r/^attempted to start without valid key/

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  describe "init/1" do
    test "raises if configured with invalid key" do
      assert_raise(Pigeon.ConfigError, @invalid_key_msg, fn ->
        [key: nil]
        |> Pigeon.FCM.init()
      end)
    end
  end

  describe "handle_push/3" do
    test "successfully sends a valid push" do
      notification =
        valid_fcm_reg_id()
        |> Notification.new(%{}, @data)
        |> PigeonTest.FCM.push()

      expected = [success: valid_fcm_reg_id()]
      assert notification.response == expected
    end

    test "successfully handles multiple registration_ids" do
      notification =
        [valid_fcm_reg_id(), "bad_reg_id"]
        |> Notification.new(%{}, @data)
        |> PigeonTest.FCM.push()

      assert notification.response[:success] == valid_fcm_reg_id()
      assert notification.response[:invalid_registration] == "bad_reg_id"
    end

    test "successfully sends a valid push with callback" do
      reg_id = valid_fcm_reg_id()
      n = Notification.new(reg_id, %{}, @data)
      pid = self()
      PigeonTest.FCM.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{response: regids}, 5000)
      assert n.status == :success
      assert regids == [success: reg_id]
    end

    test "returns an error on pushing with a bad registration_id" do
      reg_id = "bad_reg_id"
      n = Notification.new(reg_id, %{}, @data)
      pid = self()
      PigeonTest.FCM.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{}, 5000)
      assert n.status == :success
      assert n.response == [invalid_registration: reg_id]
      assert n.registration_id == reg_id
      assert n.payload == %{"data" => @data}
    end
  end
end
