defmodule Pigeon.FCMTest do
  use ExUnit.Case
  doctest Pigeon.FCM, import: true
  doctest Pigeon.FCM.Config, import: true
  doctest Pigeon.FCM.Notification, import: true

  alias Pigeon.FCM.Notification
  require Logger

  @data %{"message" => "Test push"}
  @invalid_project_msg ~r/^attempted to start without valid :project_id/
  @invalid_fetcher_msg ~r/^attempted to start without valid :token_fetcher module/

  defp valid_fcm_reg_id do
    Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]
  end

  describe "init/1" do
    test "raises if configured with invalid project" do
      assert_raise(Pigeon.ConfigError, @invalid_project_msg, fn ->
        [project_id: nil, token_fetcher: PigeonTest.Goth]
        |> Pigeon.FCM.init()
      end)
    end

    test "raises if configured with invalid token_fetcher module" do
      assert_raise(Pigeon.ConfigError, @invalid_fetcher_msg, fn ->
        [project_id: "example", token_fetcher: nil]
        |> Pigeon.FCM.init()
      end)
    end
  end

  describe "handle_push/3" do
    test "successfully sends a valid push" do
      notification =
        {:token, valid_fcm_reg_id()}
        |> Notification.new(%{}, @data)
        |> PigeonTest.FCM.push()

      assert notification.name
    end

    test "successfully sends a valid push with callback" do
      target = {:token, valid_fcm_reg_id()}
      n = Notification.new(target, %{}, @data)
      pid = self()
      PigeonTest.FCM.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{target: ^target}, 5000)
      assert n.name
      assert n.response == :success
    end

    @tag :focus
    test "successfully sends a valid push with a dynamic dispatcher" do
      target = {:token, valid_fcm_reg_id()}
      n = Notification.new(target, %{}, @data)
      pid = self()

      {:ok, dispatcher} =
        Pigeon.Dispatcher.start_link(
          Application.get_env(:pigeon, PigeonTest.FCM)
        )

      Pigeon.push(dispatcher, n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{target: ^target}, 5000)
      assert n.name
      assert n.response == :success
    end

    test "returns an error on pushing with a bad registration_id" do
      target = {:token, "bad_reg_id"}
      n = Notification.new(target, %{}, @data)
      pid = self()
      PigeonTest.FCM.push(n, on_response: fn x -> send(pid, x) end)

      assert_receive(n = %Notification{target: ^target}, 5000)
      assert n.error
      refute n.name
      assert n.response == :invalid_argument
    end

    test "responds :not_started if dispatcher not started" do
      target = {:token, valid_fcm_reg_id()}
      n = Notification.new(target, %{}, @data)
      pid = self()

      Pigeon.push(PigeonTest.NotStarted, n,
        on_response: fn x -> send(pid, x) end
      )

      assert_receive(n = %Notification{target: ^target}, 5000)
      refute n.name
      assert n.response == :not_started
    end
  end
end
