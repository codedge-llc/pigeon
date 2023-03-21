defmodule Pigeon.Pushy.Error do
  @moduledoc false

  @doc false
  @spec parse(Pigeon.Pushy.Notification.t(), map) :: Pigeon.Pushy.Notification.error_response()
  def parse(notification, error) do
    error
    |> Map.get("code")
    |> parse_response()
  end

  defp parse_response("NO_RECIPIENTS"), do: :no_recipients
  defp parse_response("NO_APNS_AUTH"), do: :no_apns_auth
  defp parse_response("PAYLOAD_LIMIT_EXCEEDED"), do: :payload_limit_exceeded
  defp parse_response("INVALID_PARAM"), do: :invalid_param
  defp parse_response("INVALID_API_KEY"), do: :invalid_api_key
  defp parse_response("AUTH_LIMIT_EXCEEDED"), do: :auth_limit_exceeded
  defp parse_response("ACCOUNT_SUSPENDED"), do: :account_suspended
  defp parse_response("RATE_LIMIT_EXCEEDED"), do: :rate_limit_exceeded
  defp parse_response("INTERNAL_SERVER_ERROR"), do: :internal_server_error
  defp parse_response(_), do: :unknown_error
end
