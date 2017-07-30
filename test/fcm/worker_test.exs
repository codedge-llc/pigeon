defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case

  alias Pigeon.FCM

  test "send malformed JSON" do
    opts = [
      name: :gonecrashing,
      key: Application.get_env(:pigeon, :test)[:fcm_key]
    ]
    {:ok, pid} = FCM.start_connection(opts)

    me = self()
    bad = {"toto", "this is not json"}
    :gen_server.cast(pid, {:push, :fcm, bad, &(send me, &1), %{}})
    assert_receive {:error, :malformed_json}, 5000

    :gen_server.cast(pid, :stop)
  end
end
