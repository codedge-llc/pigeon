defmodule Pigeon.FCM.Notification do
  @moduledoc """
  Defines FCM notification struct and convenience constructor functions.
  """
  defstruct registration_id: nil, payload: %{}, priority: :normal

  @type t :: %__MODULE__{
    payload: %{},
    registration_id: String.t | [String.t],
    priority: :normal | :high
  }

  @doc """
  Creates `FCM.Notification` struct with device registration IDs and optional
  notification and data payloads.

  ## Examples

      iex> Pigeon.FCM.Notification.new("reg ID")
      %Pigeon.FCM.Notification{
        payload: %{},
        registration_id: "reg ID",
        priority: :normal
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"})
      %Pigeon.FCM.Notification{
        payload: %{"notification" => %{"body" => "test message"}},
        registration_id: "reg ID",
        priority: :normal
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"}, %{"key" => "value"})
      %Pigeon.FCM.Notification{
        payload: %{
          "data" => %{"key" => "value"},
          "notification" => %{"body" => "test message"}
        },
        registration_id: "reg ID",
        priority: :normal
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
        payload: %{"data" => %{"key" => 1234}},
        registration_id: nil
      }
  """
  def put_data(n, data), do: update_payload(n, "data", data)

  @doc """
  Updates `"notification"` key in push payload.

  ## Examples

      iex> Pigeon.FCM.Notification.put_notification(%Pigeon.FCM.Notification{},
      ...> %{"body" => "message"})
      %Pigeon.FCM.Notification{
        payload: %{"notification" => %{"body" => "message"}},
        registration_id: nil
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
