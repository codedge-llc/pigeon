defmodule Pigeon.GCMWorker do
  @moduledoc """
    Handles all FCM request and response parsing over an HTTP2 connection.
  """
  use Pigeon.GenericH2Worker
  alias Pigeon.GCM.NotificationResponse
  require Logger

  def default_name, do: :gcm_default

  def host(config) do
    config[:endpoint] || "fcm.googleapis.com"
  end

  def port(config) do
    config[:port]     || 443
  end

  def socket_options(_config) do
    []
  end

  def req_headers(config, _notification) do
    [{"authorization", "key=#{config[:key]}"}]
  end

  def req_path(_notification) do
    "/fcm/send"
  end

  defp parse_error(_notification, _headers, body) do
    {:ok, response} = Poison.decode(body)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  defp parse_response(notification, _headers, body) do
    result = Poison.decode! body
    parse_result(notification.registration_ids, result)
  end

  def parse_result(ids, %{"results" => results}) do
    parse_result1(ids, results, %NotificationResponse{})
  end

  def parse_result1([], [], result) do
    result
  end

  def parse_result1(regid, results, result) when is_binary(regid) do
    parse_result1([regid], results, result)
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id, "registration_id" => new_regid} | rest_results],
                    %NotificationResponse{ update: update} =  resp) do

    new_updates = [{regid, new_regid} | update]
    parse_result1(reg_res, rest_results, %{resp | message_id: id, update: new_updates})
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id} | rest_results],
                    %NotificationResponse{ok: ok} = resp) do

    parse_result1(reg_res, rest_results, %{resp | message_id: id, ok: [regid | ok]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => "Unavailable"} | rest_results],
                    %NotificationResponse{retry: retry} = resp) do

    parse_result1(reg_res, rest_results, %{resp | retry: [regid | retry]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => invalid } | rest_results],
                    %NotificationResponse{remove: remove} = resp) when invalid == "NotRegistered"
                                                                    or invalid == "InvalidRegistration" do

    parse_result1(reg_res, rest_results, %{resp | remove: [regid | remove]})
  end

  def parse_result1([regid | reg_res] = regs,
                    [%{"error" => error} | rest_results] = results,
                    %NotificationResponse{error: regs_in_error} = resp) do

    case Map.has_key?(regs_in_error, error) do
      true ->
        parse_result1(reg_res, rest_results,
          %{resp | error: %{regs_in_error | error => regid}})
      false -> # create map key if required.
         parse_result1(regs, results,
          %{resp | error: Map.merge(%{error => []}, regs_in_error)})
    end
  end

  def error_msg(code, _error) do
    case code do
      "400" -> "Malformed JSON"
      "401" -> "Unauthorized"
      _ -> "Unknown"
    end
  end
end
