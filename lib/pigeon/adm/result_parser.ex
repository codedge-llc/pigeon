defmodule Pigeon.ADM.ResultParser do
  @moduledoc false

  @doc ~S"""
  Parses response and updates notification.

  ## Examples

      iex> parse(%Pigeon.ADM.Notification{}, %{}, fn(x) -> x end)
      %Pigeon.ADM.Notification{response: :success}

      iex> parse(%Pigeon.ADM.Notification{}, %{"registrationID" => "test"},
      ...> fn(x) -> x end)
      %Pigeon.ADM.Notification{response: :update,
        updated_registration_id: "test"}

      iex> parse(%Pigeon.ADM.Notification{}, %{"reason" => "InvalidRegistrationId"},
      ...> fn(x) -> x end)
      %Pigeon.ADM.Notification{response: :invalid_registration_id}
  """
  # Handle RegID updates
  def parse(notification, %{"registrationID" => new_regid}, on_response) do
    n = %{notification | response: :update, updated_registration_id: new_regid}
    on_response.(n)
  end

  def parse(notification, %{"reason" => error}, on_response) do
    error = error |> to_error_atom
    n = %{notification | response: error}
    on_response.(n)
  end

  def parse(notification, %{}, on_response) do
    n = %{notification | response: :success}
    on_response.(n)
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
