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
            response: nil

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
          response: atom | nil
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
