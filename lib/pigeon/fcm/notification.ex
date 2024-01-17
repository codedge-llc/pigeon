defmodule Pigeon.FCM.Notification do
  @moduledoc """
  Defines FCM notification struct and convenience constructor functions.
  """

  defstruct __meta__: %Pigeon.Metadata{},
            android: nil,
            apns: nil,
            data: nil,
            error: nil,
            fcm_options: nil,
            name: nil,
            notification: %{},
            response: nil,
            target: nil,
            validate_only: nil,
            webpush: nil

  @type t :: %__MODULE__{
          __meta__: Pigeon.Metadata.t(),
          android: map | nil,
          apns: map | nil,
          data: map | nil,
          error: map | nil,
          fcm_options: map | nil,
          name: binary | nil,
          notification: map | nil,
          response: atom | nil,
          target: target,
          validate_only: boolean | nil,
          webpush: map | nil
        }

  @type error_response ::
          :unspecified_error
          | :invalid_argument
          | :unregistered
          | :sender_id_mismatch
          | :quota_exceeded
          | :unavailable
          | :internal
          | :third_party_auth_error
          | :unknown_error

  @typedoc ~S"""
  FCM notification target. Must be one of the following:

  - `{:token, "string"}` - Registration token to send a message to.
  - `{:topic, "string"}` - Topic name to send a message to, e.g. "weather". 
    Note: "/topics/" prefix should not be provided.
  - `{:condition, "string"}` - Condition to send a message to, e.g. "'foo' 
    in topics && 'bar' in topics".
  """
  @type target :: {:token, binary} | {:topic, binary} | {:condition, binary}

  @doc """
  Creates `FCM.Notification` struct with given target and optional
  notification and data payloads.

  ## Examples

      iex> Pigeon.FCM.Notification.new({:token, "reg ID"})
      %Pigeon.FCM.Notification{
        data: nil,
        notification: nil,
        target: {:token, "reg ID"}
      }

      iex> Pigeon.FCM.Notification.new({:topic, "example"})
      %Pigeon.FCM.Notification{
        data: nil,
        notification: nil,
        target: {:topic, "example"}
      }

      iex> Pigeon.FCM.Notification.new({:token, "reg ID"}, %{"body" => "test message"})
      %Pigeon.FCM.Notification{
        data: nil,
        notification: %{"body" => "test message"},
        target: {:token, "reg ID"}
      }

      iex> Pigeon.FCM.Notification.new({:token, "reg ID"}, %{"body" => "test message"},
      ...> %{"key" => "value"})
      %Pigeon.FCM.Notification{
        data: %{"key" => "value"},
        notification: %{"body" => "test message"},
        target: {:token, "reg ID"}
      }
  """
  def new(target, notification \\ nil, data \\ nil)

  def new({type, _} = target, notification, data)
      when type in [:token, :topic, :condition] do
    %Pigeon.FCM.Notification{
      target: target,
      notification: notification,
      data: data
    }
  end
end

defimpl Pigeon.Encodable, for: Pigeon.FCM.Notification do
  def binary_payload(notif) do
    encode_requests(notif)
  end

  @doc false
  def encode_requests(notif) do
    message =
      %{}
      |> encode_target(notif.target)
      |> maybe_encode_attr("android", notif.android)
      |> maybe_encode_attr("apns", notif.apns)
      |> maybe_encode_attr("data", notif.data)
      |> maybe_encode_attr("fcm_options", notif.fcm_options)
      |> maybe_encode_attr("notification", notif.notification)
      |> maybe_encode_attr("webpush", notif.webpush)

    %{"message" => message}
    |> maybe_encode_attr("validate_only", notif.validate_only)
    |> Pigeon.json_library().encode!()
  end

  defp encode_target(map, {type, value}) do
    Map.put(map, to_string(type), value)
  end

  defp maybe_encode_attr(map, _key, nil), do: map

  defp maybe_encode_attr(map, key, val) do
    Map.put(map, key, val)
  end
end
