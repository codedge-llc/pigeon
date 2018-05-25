defmodule Pigeon.FCM.ResultParser do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  def parse([], [], on_response, notif) do
    process_on_response(on_response, notif)
  end

  def parse(regid, results, on_response, notif) when is_binary(regid) do
    parse([regid], results, on_response, notif)
  end

  def parse([regid | reg_res], [result | rest_results], on_response, notif) do
    updated_notif =
      case result do
        %{"message_id" => id, "registration_id" => new_regid} ->
          notif
          |> put_update(regid, new_regid)
          |> Map.put(:message_id, id)

        %{"message_id" => id} ->
          notif
          |> put_success(regid)
          |> Map.put(:message_id, id)

        %{"error" => error} ->
          notif
          |> put_error(regid, error)
      end

    parse(reg_res, rest_results, on_response, updated_notif)
  end

  defp put_update(%{response: resp} = notif, regid, new_regid) do
    new_resp = [{:update, {regid, new_regid}} | resp]
    %{notif | response: new_resp}
  end

  defp put_success(%{response: resp} = notif, regid) do
    new_resp = [{:success, regid} | resp]
    %{notif | response: new_resp}
  end

  defp put_error(%{response: resp} = notif, regid, error) do
    error = error |> Macro.underscore() |> String.to_atom()
    %{notif | response: [{error, regid} | resp]}
  end
end
