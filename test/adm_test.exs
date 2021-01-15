defmodule Pigeon.ADMTest do
  use ExUnit.Case
  doctest Pigeon.ADM, import: true

  test "initial_state/1 initializes correctly" do
    opts = [
      client_id: "1234",
      client_secret: "secret"
    ]

    expected = %{
      config: %Pigeon.ADM.Config{client_id: "1234", client_secret: "secret"},
      access_token: nil,
      access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
      access_token_expiration_seconds: 0,
      access_token_type: nil
    }

    assert Pigeon.ADM.initial_state(opts) == expected
  end

  test "handle_info/2 handles random messages" do
    assert Pigeon.ADM.handle_info("random", %Pigeon.ADM.Config{}) ==
             {:noreply, %Pigeon.ADM.Config{}}
  end
end
