defmodule Pigeon.Pushy.Notification do
  @moduledoc """
  Defines Pushy notification struct and convenience constructor functions.
  """

  defstruct __meta__: %Pigeon.Metadata{},
            to: "",
            data: %{},
            time_to_live: nil,
            content_available: nil,
            mutable_content: nil,
            notification: nil,
            schedule: nil,
            collapse_key: nil,
            response: nil,
            push_id: nil,
            success: nil,
            successful_device_count: nil,
            failed: nil

  @type t :: %__MODULE__{
          __meta__: Pigeon.Metadata.t(),
          to: String.t() | [String.t()],
          data: map,
          time_to_live: integer | nil,
          content_available: boolean | nil,
          mutable_content: boolean | nil,
          notification: map | nil,
          schedule: integer | nil,
          collapse_key: String.t() | nil,
          response: atom | nil,
          push_id: String.t() | nil,
          success: boolean() | nil,
          successful_device_count: integer() | nil,
          failed: [String.t()] | nil
        }

  @type error_response ::
          :no_recipients
          | :no_apns_auth
          | :payload_limit_exceeded
          | :invalid_param
          | :invalid_api_key
          | :auth_limit_exceeded
          | :account_suspended
          | :rate_limit_exceeded
          | :internal_server_error
          | :unknown_error

  @spec new(map, String.t() | [String.t()]) :: __MODULE__.t()
  def new(message, device_ids) do
    %__MODULE__{
      to: device_ids,
      data: message
    }
  end

  @spec put_time_to_live(__MODULE__.t(), integer()) :: __MODULE__.t()
  def put_time_to_live(notification, time_to_live) do
    %{notification | time_to_live: time_to_live
  end

  @spec put_content_available(__MODULE__.t(), boolean()) :: __MODULE__.t()
  def put_content_available(notification, content_available) do
    %{notification | content_available: content_available}
  end

  @spec put_mutable_content(__MODULE__.t(), boolean()) :: __MODULE__.t()
  def put_mutable_content(notification, mutable_content) do
    %{notification | mutable_content: mutable_content}
  end

  @spec put_notification(__MODULE__.t(), map) :: __MODULE__.t()
  def put_notification(notification, notification_details) do
    %{notification | notification: notification_details}
  end

  @spec put_schedule(__MODULE__.t(), integer) :: __MODULE__.t()
  def put_schedule(notification, schedule) do
    %{notification | schedule: schedule}
  end

  @spec put_collapse_key(__MODULE__.t(), String.t()) :: __MODULE__.t()
  def put_collapse_key(notification, collapse_key) do
    %{notification | collapse_key: collapse_key}
  end
end

defimpl Pigeon.Encodable, for: Pigeon.Pushy.Notification do
  def binary_payload(notif) do
    encode_requests(notif)
  end

  @doc false
  def encode_requests(notif) do
    %{}
    |> encode_to(notif.to)
    |> encode_data(notif.data)
    |> maybe_encode_attr("time_to_live", notif.time_to_live)
    |> maybe_encode_attr("content_available", notif.content_available)
    |> maybe_encode_attr("mutable_content", notif.mutable_content)
    |> maybe_encode_attr("notification", notif.notification)
    |> maybe_encode_attr("schedule", notif.schedule)
    |> maybe_encode_attr("collapse_key", notif.collapse_key)
    |> Pigeon.json_library().encode!()
  end

  defp encode_to(map, value) do
    Map.put(map, "to", value)
  end

  defp encode_data(map, value) do
    Map.put(map, "data", value)
  end

  defp maybe_encode_attr(map, _key, nil), do: map

  defp maybe_encode_attr(map, key, val) do
    Map.put(map, key, val)
  end
end
