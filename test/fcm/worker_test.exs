defmodule Pigeon.FCM.WorkerTest do
  use ExUnit.Case
  alias Pigeon.FCM
  alias Pigeon.FCM.{NotificationResponse}

  @data %{"message" => "Test push"}
  @payload %{"data" => @data}

  defp valid_fcm_reg_id, do: Application.get_env(:pigeon, :test)[:valid_fcm_reg_id]

  test "parse_result with success" do
    {:ok, response} =
      FCM.Worker.parse_result1(
        ["regid"],
        [%{ "message_id" => "1:0408" }],
        &(&1), %NotificationResponse{}
      )
    assert  response.ok == ["regid"]
  end

  test "parse_result with success and new registration_id" do
    {:ok, response} =
      FCM.Worker.parse_result1(
        ["regid"],
        [%{ "message_id" => "1:2342", "registration_id" => "32" }],
        &(&1), %NotificationResponse{}
      )

    assert response.update == [{"regid", "32"}]
    assert response.message_id == "1:2342"
  end

  test "parse_result with error unavailable" do
    {:ok, response} =
      FCM.Worker.parse_result1(
        ["regid"],
        [%{ "error" => "Unavailable" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.retry == ["regid"]
  end

  test "parse_result with custom error" do
    {:ok, response} =
      FCM.Worker.parse_result1(
        ["regid"],
        [%{ "error" => "CustomError" }],
        &(&1),
        %NotificationResponse{}
      )
    assert response.error == %{"CustomError" => "regid"}
  end

  test "send malformed JSON" do
    #{:ok, pid} = FCM.Worker.start_link(:gonecrashing, key: Application.get_env(:pigeon, :fcm)[:key])
    opts = [name: :gonecrashing, key: Application.get_env(:pigeon, :test)[:fcm_key]]
    {:ok, pid} = FCM.start_connection(opts)

    me = self()
    :gen_server.cast(pid, {:push, :fcm, {"toto", "this is not json"}, &(send me, &1), %{}})
    assert_receive {:error, :malformed_json}, 5000
    :gen_server.cast(pid, :stop)
  end
end
