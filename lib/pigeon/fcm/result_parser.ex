defmodule Pigeon.FCM.ResultParser do
  @moduledoc false

  alias Pigeon.FCM.Notification

  def parse([], [], on_response, notif) do
    Task.Supervisor.start_child(Pigeon.Tasks, fn -> on_response.(notif) end)
  end

  def parse(regid, results, on_response, notif) when is_binary(regid) do
    parse([regid], results, on_response, notif)
  end

  # Handle RegID updates
  def parse([regid | reg_res],
            [%{"message_id" => id, "registration_id" => new_regid} | rest],
            on_response,
            %Notification{response: resp} = notif) do

    new_resp = [{:update, {regid, new_regid}} | resp]
    notif = %{notif | message_id: id, response: new_resp}
    parse(reg_res, rest, on_response, notif)
  end

  # Handle successful RegIDs, also parse `message_id`
  def parse([regid | reg_res],
            [%{"message_id" => id} | rest_results],
            on_response,
            %Notification{response: resp} = notif) do

    new_resp = [{:success, regid} | resp]
    n = %{notif | message_id: id, response: new_resp}
    parse(reg_res, rest_results, on_response, n)
  end

  # Handle error RegIDs
  def parse([regid | reg_res],
            [%{"error" => error} | rest_results],
            on_response,
            %Notification{response: resp} = notif) do

    error = error |> Macro.underscore |> String.to_atom
    n = %{notif | response: [{error, regid} | resp]}
    parse(reg_res, rest_results, on_response, n)
  end
end
