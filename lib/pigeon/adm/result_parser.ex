defmodule Pigeon.ADM.ResultParser do
  @moduledoc false

  alias Pigeon.ADM.NotificationResponse

  def parse([], [], on_response, result) do
    on_response.({:ok, result})
  end

  def parse(regid, results, on_response, result) when is_binary(regid) do
    parse([regid], results, on_response, result)
  end

  # Handle RegID updates
  def parse([regid | reg_res],
            [%{"registrationID" => new_regid} | rest_results],
            on_response,
            %NotificationResponse{update: update} = resp) do

    new_updates = [{regid, new_regid} | update]
    parse(reg_res, rest_results, on_response, %{resp | update: new_updates})
  end

  # Retry `MaxRateExceeded` RegIDs
  def parse([regid | reg_res],
            [%{"reason" => "MaxRateExceeded"} | rest_results],
            on_response,
            %NotificationResponse{retry: retry} = resp) do

    parse(reg_res, rest_results, on_response, %{resp | retry: [regid | retry]})
  end

  # Remove `Unregistered` or `InvalidRegistrationId` RegIDs
  def parse([regid | reg_res],
            [%{"reason" => invalid} | rest_results],
            on_response,
            %NotificationResponse{remove: remove} = resp) when invalid == "Unregistered"
                                                            or invalid == "InvalidRegistrationId" do

    parse(reg_res, rest_results, on_response, %{resp | remove: [regid | remove]})
  end

  # Handle all other error keys
  def parse([regid | reg_res] = regs,
            [%{"reason" => error} | rest_results] = results,
            on_response,
            %NotificationResponse{error: regs_in_error} = resp) do

    if Map.has_key?(regs_in_error, error) do
      parse(reg_res, rest_results, on_response, %{resp | error: %{regs_in_error | error => regid}})
    else
      # create map key if required.
      parse(regs, results, on_response, %{resp | error: Map.merge(%{error => []}, regs_in_error)})
    end
  end

  # Handle successful RegIDs
  def parse([regid | reg_res],
            [%{} | rest_results],
            on_response,
            %NotificationResponse{ok: ok} = resp) do

    parse(reg_res, rest_results, on_response, %{resp | ok: [regid | ok]})
  end
end
