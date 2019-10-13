defmodule Pigeon.APNS.Notification do
  @moduledoc ~S"""
  Defines APNS notification struct and constructor functions.
  """

  defstruct collapse_id: nil,
            device_token: nil,
            expiration: nil,
            priority: nil,
            push_type: "alert",
            id: nil,
            payload: %{"aps" => %{}},
            topic: nil,
            response: nil

  alias Pigeon.APNS.{Error, Notification}

  @typedoc ~S"""
  APNS notification

  ## Examples

      %Pigeon.APNS.Notification{
          collapse_id: nil,
          device_token: "device token",
          expiration: nil,
          priority: nil,
          push_type: "alert",
          id: nil, # Set on push response if nil
          payload: %{"aps" => %{"alert" => "push message"}},
          response: nil, # Set on push response
          topic: "com.example.YourApp"
      }
  """
  @type t :: %__MODULE__{
          collapse_id: String.t() | nil,
          device_token: String.t() | nil,
          expiration: non_neg_integer | nil,
          priority: non_neg_integer | nil,
          push_type: String.t() | nil,
          id: String.t() | nil,
          payload: %{String.t() => String.t()},
          response: response,
          topic: String.t() | nil
        }

  @typedoc ~S"""
  APNS push response

  - nil - Push has not been sent yet
  - `:success` - Push was successfully sent
  - `t:Pigeon.APNS.Error.error_response/0` - Push attempted but
     server responded with error
  - `:timeout` - Internal error. Push did not reach APNS servers
  """
  @type response :: nil | :success | Error.error_response() | :timeout

  @doc """
  Returns an `APNS.Notification` struct with given message, device token, and
  topic (optional).

  Push payload is constructed in the form of `%{"aps" => %{"alert" => msg}}`

  ## Examples

      iex> Pigeon.APNS.Notification.new("push message", "device token")
      %Pigeon.APNS.Notification{
        device_token: "device token",
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: nil
      }
  """
  @spec new(String.t() | map, String.t(), String.t() | nil) :: t
  def new(msg, token, topic \\ nil) do
    %Notification{
      device_token: token,
      payload: %{"aps" => %{"alert" => msg}},
      topic: topic
    }
  end

  @doc """
  Returns an `APNS.Notification` struct with given message, device token,
  topic, and message ID.

  Push payload is constructed in the form of `%{"aps" => %{"alert" => msg}}`

  ## Examples

      iex> Pigeon.APNS.Notification.new("push message", "device token", "topic", "id_1234")
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: "device token",
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: "id_1234",
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: "topic"
      }
  """
  @spec new(String.t() | map, String.t(), String.t(), String.t()) :: t
  def new(msg, token, topic, id) do
    %Notification{
      device_token: token,
      id: id,
      payload: %{"aps" => %{"alert" => msg}},
      topic: topic
    }
  end

  @doc """
  Updates `"alert"` key in push payload.

  This is the alert message displayed on the device.

  ## Examples

      iex> Pigeon.APNS.Notification.put_alert(%Pigeon.APNS.Notification{}, "push message")
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: nil
      }
  """
  @spec put_alert(t, String.t() | map) :: t
  def put_alert(notification, alert),
    do: update_payload(notification, "alert", alert)

  @doc """
  Updates `"badge"` key in push payload.

  This is the badge number displayed on the application.

  ## Examples

      iex> Pigeon.APNS.Notification.put_badge(%Pigeon.APNS.Notification{}, 5)
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"badge" => 5}},
        topic: nil
      }
  """
  @spec put_badge(t, integer) :: t
  def put_badge(notification, badge),
    do: update_payload(notification, "badge", badge)

  @doc """
  Updates `"sound"` key in push payload.

  Used for custom notification sounds. Value should
  be the name of the custom sound file in the application's binary.

  ## Examples

      iex> Pigeon.APNS.Notification.put_sound(%Pigeon.APNS.Notification{}, "custom.aiff")
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"sound" => "custom.aiff"}},
        topic: nil
      }
  """
  @spec put_sound(t, String.t()) :: t
  def put_sound(notification, sound),
    do: update_payload(notification, "sound", sound)

  @doc """
  Sets `"content-available"` flag in push payload.

  Used for silent notifications. When set, ensure `alert`, `badge`, and `sound` keys
  are not configured.

  ## Examples

      iex> Pigeon.APNS.Notification.put_content_available(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"content-available" => 1}},
        topic: nil
      }
  """
  @spec put_content_available(t) :: t
  def put_content_available(notification),
    do: update_payload(notification, "content-available", 1)

  @doc """
  Updates `"category"` key in push payload.

  ## Examples

      iex> Pigeon.APNS.Notification.put_category(%Pigeon.APNS.Notification{}, "category")
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"category" => "category"}},
        topic: nil
      }
  """
  @spec put_category(t, String.t()) :: t
  def put_category(notification, category),
    do: update_payload(notification, "category", category)

  @doc """
  Sets `"mutable-content"` flag in push payload.

  Used for notification service extensions (such as displaying custom media).

  ## Examples

      iex> Pigeon.APNS.Notification.put_mutable_content(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: nil,
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"mutable-content" => 1}},
        topic: nil
      }
  """
  @spec put_mutable_content(t) :: t
  def put_mutable_content(notification),
    do: update_payload(notification, "mutable-content", 1)

  defp update_payload(notification, key, value) do
    new_aps =
      notification.payload
      |> Map.get("aps")
      |> Map.put(key, value)

    new_payload = notification.payload |> Map.put("aps", new_aps)
    %{notification | payload: new_payload}
  end

  @doc """
  Puts custom data in push payload.

  ## Examples

      iex> n = Pigeon.APNS.Notification.new("test message", "device token")
      iex> Pigeon.APNS.Notification.put_custom(n, %{"custom-key" => 1234})
      %Pigeon.APNS.Notification{
        collapse_id: nil,
        device_token: "device token",
        expiration: nil,
        priority: nil,
        push_type: "alert",
        id: nil,
        payload: %{"aps" => %{"alert" => "test message"}, "custom-key" => 1234},
        topic: nil
      }
  """
  @spec put_custom(t, %{String.t() => String.t()}) :: t
  def put_custom(notification, data) do
    new_payload = Map.merge(notification.payload, data)
    %{notification | payload: new_payload}
  end
end
