defmodule Pigeon.FCM.Notification do
  @moduledoc """
  Defines FCM notification struct and convenience constructor functions.
  """
  defstruct registration_id: nil, payload: %{}, message_id: nil,
            updated_registration_id: nil, priority: :normal

  @type t :: %__MODULE__{
    message_id: String.t,
    payload: %{},
    registration_id: String.t | [String.t],
    updated_registration_id: String.t
  }

  @doc """
  Creates `FCM.Notification` struct with device registration IDs and optional
  notification and data payloads.

  ## Examples

      iex> Pigeon.FCM.Notification.new("reg ID")
      %Pigeon.FCM.Notification{
        message_id: nil,
        payload: %{},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"})
      %Pigeon.FCM.Notification{
        message_id: nil,
        payload: %{"notification" => %{"body" => "test message"}},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"}, %{"key" => "value"})
      %Pigeon.FCM.Notification{
        message_id: nil,
        payload: %{
          "data" => %{"key" => "value"},
          "notification" => %{"body" => "test message"}
        },
        registration_id: "reg ID",
        updated_registration_id: nil
      }
  """
  def new(registration_ids, notification \\ %{}, data \\ %{})
  def new(registration_ids, notification, data) do
    %Pigeon.FCM.Notification{registration_id: registration_ids}
    |> put_notification(notification)
    |> put_data(data)
  end

  @doc """
  Updates `"data"` key in push payload.

  ## Examples

      iex> Pigeon.FCM.Notification.put_data(%Pigeon.FCM.Notification{}, %{"key" => 1234})
      %Pigeon.FCM.Notification{
        message_id: nil,
        payload: %{"data" => %{"key" => 1234}},
        registration_id: nil,
        updated_registration_id: nil
      }
  """
  def put_data(n, data), do: update_payload(n, "data", data)

  @doc """
  Updates `"notification"` key in push payload.

  ## Examples

      iex> Pigeon.FCM.Notification.put_notification(%Pigeon.FCM.Notification{},
      ...> %{"body" => "message"})
      %Pigeon.FCM.Notification{
        message_id: nil,
        payload: %{"notification" => %{"body" => "message"}},
        registration_id: nil,
        updated_registration_id: nil
      }
  """
  def put_notification(n, notification), do: update_payload(n, "notification", notification)

  def put_priority(n, :normal), do: %{n | priority: :normal}
  def put_priority(n, :high),   do: %{n | priority: :high}
  def put_priority(n, _),       do: n

  defp update_payload(notification, _key, value) when value == %{}, do: notification
  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)
    %{notification | payload: payload}
  end
end

defmodule Pigeon.FCM.NotificationResponse do
  @moduledoc """
  Passed to the FCM on_response callback
  """
  defstruct message_id: nil, ok: [], retry: [], update: [], remove: [], error: %{}
end
