defmodule Pigeon.FCM.ResultParser do
  @moduledoc false

  alias Pigeon.FCM.NotificationResponse

  def parse([], [], on_response, result) do
    on_response.({:ok, result})
  end

  def parse(regid, results, on_response, result) when is_binary(regid) do
    parse([regid], results, on_response, result)
  end

  # Handle RegID updates
  def parse([regid | reg_res],
            [%{"message_id" => id, "registration_id" => new_regid} | rest_results],
            on_response,
            %NotificationResponse{update: update} =  resp) do

    new_updates = [{regid, new_regid} | update]
    parse(reg_res, rest_results, on_response, %{resp | message_id: id, update: new_updates})
  end

  # Handle successful RegIDs, also parse `message_id`
  def parse([regid | reg_res],
            [%{"message_id" => id} | rest_results],
            on_response,
            %NotificationResponse{ok: ok} = resp) do

    parse(reg_res, rest_results, on_response, %{resp | message_id: id, ok: [regid | ok]})
  end

  # Retry `Unavailable` RegIDs
  def parse([regid | reg_res],
            [%{"error" => "Unavailable"} | rest_results],
            on_response,
            %NotificationResponse{retry: retry} = resp) do

    parse(reg_res, rest_results, on_response, %{resp | retry: [regid | retry]})
  end

  # Remove `NotRegistered` or `InvalidRegistration` RegIDs
  def parse([regid | reg_res],
            [%{"error" => invalid} | rest_results],
            on_response,
            %NotificationResponse{remove: remove} = resp) when invalid == "NotRegistered"
                                                            or invalid == "InvalidRegistration" do

    parse(reg_res, rest_results, on_response, %{resp | remove: [regid | remove]})
  end

  # Handle all other error keys
  def parse([regid | reg_res] = regs,
            [%{"error" => error} | rest_results] = results,
            on_response,
            %NotificationResponse{error: regs_in_error} = resp) do

    if Map.has_key?(regs_in_error, error) do
      parse(reg_res, rest_results, on_response, %{resp | error: %{regs_in_error | error => regid}})
    else
      # create map key if required.
      parse(regs, results, on_response, %{resp | error: Map.merge(%{error => []}, regs_in_error)})
    end
  end
end
