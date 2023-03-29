defmodule Pigeon.Pushy.Notification do
  @moduledoc """
  Defines Pushy notification struct and convenience constructor functions.

  For more information on Pushy notification requests, see
  https://pushy.me/docs/api/send-notifications.
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

  @typedoc """
  Pushy notification

  ## Examples
      %Pigeon.Pushy.Notification{
          __meta__: %Pigeon.Metadata{on_response: nil},
          to: "device token or topic",
          data: %{"message" => "hello world"},
          time_to_live: nil,
          content_available: nil,
          mutable_content: nil,
          notification: nil,
          schedule: nil,
          collapse_key: nil,
          response: nil, # Set on push response
          success: nil, # Set on push response
          push_id: nil, # Set on push response
          successful_device_count: nil, # Set on push response
          failed: nil # Set on push response
      }
  """
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
          response: response,
          push_id: String.t() | nil,
          success: boolean() | nil,
          successful_device_count: integer() | nil,
          failed: [String.t()] | nil
        }

  @typedoc """
  Pushy push response

  - nil - Push has not been sent yet
  - `:success` - Push was successfully sent.
  - `:failure` - If we don't receive an error code, but the response is not successful,
     this message is returned.
  - `t:Pigeon.Pushy.Error.error_response/0` - Push attempted
     server responded with error.
  - `:timeout` - Internal error. Push did not reach Pushy servers.
  """
  @type response :: nil | :success | :failure | error_response | :timeout

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

  @doc """
  Returns a `Pushy.Notification` struct with the given message and destination.

  A destination could be either a single destination, or a list of them. A destination
  is either a device token OR a topic (prefix topic names with '/topics/').

  ## Examples

      iex> Pigeon.Pushy.Notification.new(%{"message" => "Hello, world!"}, "device token")
      %Pigeon.APNS.Notification{
        __meta__: %Pigeon.Metadata{},
        to: "device token",
        data: %{"message" => "Hello, world!"},
        time_to_live: nil,
        content_available: nil,
        mutable_content: nil,
        notification: nil,
        schedule: nil,
        collapse_key: nil,
        response: nil,
        success: nil,
        push_id: nil,
        successful_device_count: nil,
        failed: nil
    }
  """
  @spec new(String.t() | [String.t()], map()) :: __MODULE__.t()
  def new(device_ids, message \\ nil)

  def new(device_ids, message) do
    %__MODULE__{
      to: device_ids,
      data: message
    }
  end

  @doc """
  Adds or updates the `data` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_data(notification, %{"message" => "some message"})
      %Pigeon.Pushy.Notification{... data: %{"message" => "some message"} ...}
  """
  @spec put_data(__MODULE__.t(), map()) :: __MODULE__.t()
  def put_data(notification, data) do
    %{notification | data: data}
  end

  @doc """
  Adds or updates the `time_to_live` field in the notification.

  This is how long (in seconds) the push notification should be kept if the device is offline.
  If unspecified, notifications will be kept for 30 days. The maximum value is 365 days.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_time_to_live(notification, 3600)
      %Pigeon.Pushy.Notification{... time_to_live: 3600 ...}
  """
  @spec put_time_to_live(__MODULE__.t(), integer()) :: __MODULE__.t()
  def put_time_to_live(notification, time_to_live) do
    %{notification | time_to_live: time_to_live}
  end

  @doc """
  Adds or updates the `content_available` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_content_available(notification, true)
      %Pigeon.Pushy.Notification{... content_available: true ...}
  """
  @spec put_content_available(__MODULE__.t(), boolean()) :: __MODULE__.t()
  def put_content_available(notification, content_available) do
    %{notification | content_available: content_available}
  end

  @doc """
  Adds or updates the `mutable_content` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_mutable_content(notification, false)
      %Pigeon.Pushy.Notification{... mutable_content: false ...}
  """
  @spec put_mutable_content(__MODULE__.t(), boolean()) :: __MODULE__.t()
  def put_mutable_content(notification, mutable_content) do
    %{notification | mutable_content: mutable_content}
  end

  @doc """
  Adds or updates the `notification` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_notification(notification, %{"title" => "New message", "body" => "Hello"})
      %Pigeon.Pushy.Notification{... notification: %{"title" => "New message", "body" => "Hello"} ...}
  """
  @spec put_notification(__MODULE__.t(), map) :: __MODULE__.t()
  def put_notification(notification, notification_details) do
    %{notification | notification: notification_details}
  end

  @doc """
  Adds or updates the `schedule` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_schedule(notification, 1648686700)
      %Pigeon.Pushy.Notification{... schedule: 1648686700 ...}
  """
  @spec put_schedule(__MODULE__.t(), integer) :: __MODULE__.t()
  def put_schedule(notification, schedule) do
    %{notification | schedule: schedule}
  end

  @doc """
  Adds or updates the `collapse_key` field in the notification.

  ## Examples

      iex> Pigeon.Pushy.Notification.put_collapse_key(notification, "new_message")
      %Pigeon.Pushy.Notification{... collapse_key: "new_message" ...}
  """
  @spec put_collapse_key(__MODULE__.t(), String.t()) :: __MODULE__.t()
  def put_collapse_key(notification, collapse_key) do
    %{notification | collapse_key: collapse_key}
  end
end
