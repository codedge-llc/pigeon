defmodule Pigeon.GCMWorkerTest do
  use ExUnit.Case
  alias Pigeon.GCMWorker

  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  defp valid_gcm_reg_id, do: Application.get_env(:pigeon, :test)[:valid_gcm_reg_id]

  test "parse_result with success" do
    assert GCMWorker.parse_result1(["regid"],[%{ "message_id" => "1:0408" }], &(&1), []) == {:ok, [{:ok, "1:0408", "regid"}]}
  end

  test "parse_result with success and new registration_id" do
    assert GCMWorker.parse_result1(["regid"], [%{ "message_id" => "1:2342", "registration_id" => "32" }], &(&1), [])  ==  {:ok, [{:update, "1:2342", "regid",  "32"}]}
  end

  test "parse_result with error unavailable" do
    assert GCMWorker.parse_result1(["regid"], [%{ "error" => "Unavailable" }], &(&1), [])  =={:ok, [ {:retry, "regid"}]}
  end 

  test "send malformed JSON" do
    {:ok, pid} = GCMWorker.start_link(:gonecrashing, key: Application.get_env(:pigeon, :gcm)[:key])
    me = self()
    :gen_server.cast(pid, {:push, :gcm, {"toto", "this is not json"}, &(send me, &1)})
    assert_receive {:error, :malformed_json}, 5000
    :gen_server.cast(pid, :stop)
  end

end
