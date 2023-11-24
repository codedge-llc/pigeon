defmodule Pigeon.APNS.Notification do
  @moduledoc ~S"""
  Defines APNS notification struct and constructor functions.
  """

  defstruct __meta__: %Pigeon.Metadata{},
            collapse_id: nil,
            device_token: nil,
            expiration: nil,
            id: nil,
            payload: %{"aps" => %{}},
            priority: nil,
            push_type: "alert",
            topic: nil,
            response: nil

  @typedoc ~S"""
  APNS notification

  ## Examples

      %Pigeon.APNS.Notification{
          __meta__: %Pigeon.Metadata{on_response: nil},
          collapse_id: nil,
          device_token: "device token",
          expiration: nil,
          id: nil, # Set on push response if nil
          payload: %{"aps" => %{"alert" => "push message"}},
          priority: nil,
          push_type: "alert",
          response: nil, # Set on push response
          topic: "com.example.YourApp"
      }
  """
  @type t :: %__MODULE__{
          __meta__: Pigeon.Metadata.t(),
          collapse_id: String.t() | nil,
          device_token: String.t() | nil,
          expiration: non_neg_integer | nil,
          id: String.t() | nil,
          payload: %{String.t() => term},
          priority: non_neg_integer | nil,
          push_type: String.t() | nil,
          response: response,
          topic: String.t() | nil
        }

  @typedoc ~S"""
  APNS push response

  - nil - Push has not been sent yet.
  - `:success` - Push was successfully sent.
  - `t:Pigeon.APNS.Error.error_response/0` - Push attempted but
     server responded with error.
  - `:timeout` - Internal error. Push did not reach APNS servers.
  """
  @type response :: nil | :success | error_response | :timeout

  @type error_response ::
          :bad_collapse_id
          | :bad_device_token
          | :bad_expiration_date
          | :bad_message_id
          | :bad_priority
          | :bad_topic
          | :device_token_not_for_topic
          | :duplicate_headers
          | :idle_timeout
          | :invalid_push_type
          | :missing_device_token
          | :missing_topic
          | :payload_empty
          | :topic_disallowed
          | :bad_certificate
          | :bad_certificate_environment
          | :expired_provider_token
          | :forbidden
          | :invalid_provider_token
          | :missing_provider_token
          | :bad_path
          | :method_not_allowed
          | :expired_token
          | :unregistered
          | :payload_too_large
          | :too_many_provider_token_updates
          | :too_many_requests
          | :internal_server_error
          | :service_unavailable
          | :shutdown
          | :unknown_error

  @doc """
  Returns an `APNS.Notification` struct with given message, device token, and
  topic (optional).

  Push payload is constructed in the form of `%{"aps" => %{"alert" => msg}}`

  ## Examples

      iex> Pigeon.APNS.Notification.new("push message", "device token")
      %Pigeon.APNS.Notification{
        __meta__: %Pigeon.Metadata{},
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
    %__MODULE__{
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
    %__MODULE__{
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
        payload: %{"aps" => %{"alert" => "push message"}}
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
        payload: %{"aps" => %{"badge" => 5}}
      }
  """
  @spec put_badge(t, integer) :: t
  def put_badge(notification, badge),
    do: update_payload(notification, "badge", badge)

  @doc """
  Updates `"category"` key in push payload.

  ## Examples

      iex> Pigeon.APNS.Notification.put_category(%Pigeon.APNS.Notification{}, "category")
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"category" => "category"}}
      }
  """
  @spec put_category(t, String.t()) :: t
  def put_category(notification, category),
    do: update_payload(notification, "category", category)

  @doc """
  Sets `"content-available"` flag in push payload.

  Used for silent notifications. When set, ensure `alert`, `badge`, and `sound` keys
  are not configured.

  ## Examples

      iex> Pigeon.APNS.Notification.put_content_available(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"content-available" => 1}}
      }
  """
  @spec put_content_available(t) :: t
  def put_content_available(notification),
    do: update_payload(notification, "content-available", 1)

  @doc """
  Puts custom data in push payload.

  ## Examples

      iex> n = Pigeon.APNS.Notification.new("test message", "device token")
      iex> Pigeon.APNS.Notification.put_custom(n, %{"custom-key" => 1234})
      %Pigeon.APNS.Notification{
        device_token: "device token",
        payload: %{"aps" => %{"alert" => "test message"}, "custom-key" => 1234}
      }
  """
  @spec put_custom(t, %{String.t() => term}) :: t
  def put_custom(notification, data) do
    new_payload = Map.merge(notification.payload, data)
    %{notification | payload: new_payload}
  end

  @doc """
  Sets `"mutable-content"` flag in push payload.

  Used for notification service extensions (such as displaying custom media).

  ## Examples

      iex> Pigeon.APNS.Notification.put_mutable_content(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"mutable-content" => 1}}
      }
  """
  @spec put_mutable_content(t) :: t
  def put_mutable_content(notification),
    do: update_payload(notification, "mutable-content", 1)

  @doc """
  Updates `"sound"` key in push payload.

  Used for custom notification sounds. Value should
  be the name of the custom sound file in the application's binary.

  ## Examples

      iex> Pigeon.APNS.Notification.put_sound(%Pigeon.APNS.Notification{}, "custom.aiff")
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"sound" => "custom.aiff"}}
      }

      iex> Pigeon.APNS.Notification.put_sound(%Pigeon.APNS.Notification{}, %{
      ...>   "critical" => 1,
      ...>   "sound" => "default",
      ...>   "volume" => 1.0
      ...> })
      %Pigeon.APNS.Notification{
        payload: %{
          "aps" => %{
            "sound" => %{
              "critical" => 1,
              "sound" => "default",
              "volume" => 1.0
            }
          }
        }
      }
  """
  @spec put_sound(t, String.t() | %{String.t() => term}) :: t
  def put_sound(notification, sound),
    do: update_payload(notification, "sound", sound)

  @doc """
  Updates `"target-content-id"` key in push payload.

  Used for bringing a specific window forward.

  ## Examples

      iex> Pigeon.APNS.Notification.put_target_content_id(%Pigeon.APNS.Notification{}, "example")
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"target-content-id" => "example"}}
      }
  """
  @spec put_target_content_id(t, String.t()) :: t
  def put_target_content_id(notification, id),
    do: update_payload(notification, "target-content-id", id)

  @doc """
  Updates `"thread-id"` key in push payload.

  Used for grouping related notifications.

  ## Examples

      iex> Pigeon.APNS.Notification.put_thread_id(%Pigeon.APNS.Notification{}, "example")
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"thread-id" => "example"}}
      }
  """
  @spec put_thread_id(t, String.t()) :: t
  def put_thread_id(notification, id),
    do: update_payload(notification, "thread-id", id)

  @doc """
  Updates `"interruption-level"` key in push payload.

  Used for managing time sensitive notifications

  ## Examples

      iex> Pigeon.APNS.Notification.put_interruption_level(%Pigeon.APNS.Notification{}, "time-sensitive")
      %Pigeon.APNS.Notification{
        payload: %{"aps" => %{"interruption-level" => "time-sensitive"}}
      }
  """
  @spec put_interruption_level(t, String.t()) :: t
  def put_interruption_level(notification, level),
    do: update_payload(notification, "interruption-level", level)

  defp update_payload(notification, key, value) do
    new_aps =
      notification.payload
      |> Map.get("aps")
      |> Map.put(key, value)

    new_payload = Map.put(notification.payload, "aps", new_aps)
    %{notification | payload: new_payload}
  end
end
