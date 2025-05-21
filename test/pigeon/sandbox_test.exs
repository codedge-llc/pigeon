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

  test "handles any kind of map with __meta__" do
    n = %{__meta__: %Pigeon.Metadata{}, expect_the: "unexpected"}
    PigeonTest.Sandbox.push([n, n], on_response: nil)
    # Didn't crash, nothing to test.
  end

  test "handle_info/2 handles unexpected messages" do
    assert Pigeon.Sandbox.handle_info(:unexpected, %{}) == {:noreply, %{}}
  end
end
