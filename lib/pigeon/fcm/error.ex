defmodule Pigeon.FCM.Error do
  @moduledoc false

  alias Pigeon.FCM.Notification

  @doc false
  @spec parse(map) :: Notification.error_response()
  def parse(%{"details" => [%{"errorCode" => error_code}]}),
    do: parse_response(error_code)

  def parse(error) do
    error
    |> Map.get("status")
    |> parse_response()
  end

  defp parse_response("UNSPECIFIED_ERROR"), do: :unspecified_error
  defp parse_response("INVALID_ARGUMENT"), do: :invalid_argument
  defp parse_response("UNREGISTERED"), do: :unregistered
  defp parse_response("SENDER_ID_MISMATCH"), do: :sender_id_mismatch
  defp parse_response("QUOTA_EXCEEDED"), do: :quota_exceeded
  defp parse_response("UNAVAILABLE"), do: :unavailable
  defp parse_response("INTERNAL"), do: :internal
  defp parse_response("THIRD_PARTY_AUTH_ERROR"), do: :third_party_auth_error
  defp parse_response(_other), do: :unknown_error
end
