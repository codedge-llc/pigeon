defmodule Pigeon.ADM.ResultParser do
  @moduledoc false

  @doc ~S"""
  Parses response and updates notification.

  ## Examples

      iex> parse(%Pigeon.ADM.Notification{}, %{})
      %Pigeon.ADM.Notification{response: :success}

      iex> parse(%Pigeon.ADM.Notification{}, %{"registrationID" => "test"})
      %Pigeon.ADM.Notification{response: :update,
        updated_registration_id: "test"}

      iex> parse(%Pigeon.ADM.Notification{}, %{"reason" => "InvalidRegistrationId"})
      %Pigeon.ADM.Notification{response: :invalid_registration_id}
  """
  # Handle RegID updates
  def parse(notification, %{"registrationID" => new_regid}) do
    notification
    |> Map.put(:response, :update)
    |> Map.put(:updated_registration_id, new_regid)
  end

  def parse(notification, %{"reason" => error}) do
    notification
    |> Map.put(:response, to_error_atom(error))
  end

  def parse(notification, %{}) do
    notification
    |> Map.put(:response, :success)
  end

  defp to_error_atom("InvalidRegistrationId"), do: :invalid_registration_id
  defp to_error_atom("InvalidData"), do: :invalid_data
  defp to_error_atom("InvalidConsolidationKey"), do: :invalid_consolidation_key
  defp to_error_atom("InvalidExpiration"), do: :invalid_expiration
  defp to_error_atom("InvalidChecksum"), do: :invalid_checksum
  defp to_error_atom("InvalidType"), do: :invalid_type
  defp to_error_atom("Unregistered"), do: :unregistered
  defp to_error_atom("AccessTokenExpired"), do: :access_token_expired
  defp to_error_atom("MessageTooLarge"), do: :message_too_large
  defp to_error_atom("MaxRateExceeded"), do: :max_rate_exceeded
  defp to_error_atom(_), do: :unknown_error
end
