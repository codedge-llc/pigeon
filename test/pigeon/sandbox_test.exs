defmodule Pigeon.SandboxTest do
  use ExUnit.Case

  test "marks nil :response as :success" do
    n =
      {:token, "test"}
      |> Pigeon.FCM.Notification.new()
      |> PigeonTest.Sandbox.push()

    assert n.response == :success
  end

  test "preserves not nil :response as is" do
    n =
      {:token, "test"}
      |> Pigeon.FCM.Notification.new()
      |> Map.put(:response, :timeout)
      |> PigeonTest.Sandbox.push()

    assert n.response == :timeout
  end

  test "handles any kind of term" do
    n = PigeonTest.Sandbox.push("unexpected")
    assert n == "unexpected"
  end
end
