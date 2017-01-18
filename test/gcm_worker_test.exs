defmodule Pigeon.GCMWorkerTest do
  use ExUnit.Case
  alias Pigeon.GCMWorker
  alias Pigeon.GCM.NotificationResponse

  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  defp valid_gcm_reg_id, do: Application.get_env(:pigeon, :test)[:valid_gcm_reg_id]

  test "parse_result with success" do
    {:ok, response} =
      GCMWorker.parse_result1(
        ["regid"],
        [%{ "message_id" => "1:0408" }],
        &(&1), %NotificationResponse{}
      )
    assert  response.ok == ["regid"]
  end

  test "parse_result with success and new registration_id" do
    {:ok, response} =
      GCMWorker.parse_result1(
        ["regid"],
        [%{ "message_id" => "1:2342", "registration_id" => "32" }],
        &(&1), %NotificationResponse{}
      )

    assert response.update == [{"regid", "32"}]
    assert response.message_id == "1:2342"
  end

  test "parse_result with error unavailable" do
    {:ok, response} =
      GCMWorker.parse_result1(
        ["regid"],
        [%{ "error" => "Unavailable" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.retry == ["regid"]
  end

  test "parse_result with custom error" do
    {:ok, response} =
      GCMWorker.parse_result1(
        ["regid"],
        [%{ "error" => "CustomError" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.error == %{"CustomError" => "regid"}
  end

  test "send malformed JSON" do
    {:ok, pid} = GCMWorker.start_link(:gonecrashing, key: Application.get_env(:pigeon, :gcm)[:key])
    me = self()
    :gen_server.cast(pid, {:push, :gcm, {"toto", "this is not json"}, &(send me, &1), %{}})
    assert_receive {:error, :malformed_json}, 5000
    :gen_server.cast(pid, :stop)
  end

end
