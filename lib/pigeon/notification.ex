defmodule Pigeon.Notification do
  @moduledoc """
  Contains shared functions for GCM and APNS.
  """
  require Logger

  def json_payload(payload) do
    response = Poison.encode(payload)
    case response do
    {:ok, result} ->
      result
    {:error, error} ->
      Logger.error error
    end
  end
end

defmodule Pigeon.APNS.Notification do
  @moduledoc """
  Defines APNS notification struct and convenience constructor functions.
  """
  defstruct device_token: nil, payload: %{"aps" => %{}}, expiration: nil, topic: nil, id: nil

  @type t :: %__MODULE__{
    device_token: String.t,
    expiration: String.t,
    id: String.t,
    payload: %{},
    topic: String.t
  }

  @doc """
  Returns an `APNS.Notification` struct with given message, device token, and
  topic (optional).

  Push payload is constructed in the form of `%{"aps" => %{"alert" => msg}}`

  ## Examples
  
      iex> Pigeon.APNS.Notification.new("push message", "device token")
      %Pigeon.APNS.Notification{
        device_token: "device token",
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: nil
      }
  """
  @spec new(String.t, String.t, String.t | nil) :: t
  def new(msg, token, topic \\ nil) do
    %Pigeon.APNS.Notification{
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
        device_token: "device token",
        expiration: nil,
        id: "id_1234",
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: "topic" 
      }
  """
  @spec new(String.t, String.t, String.t, String.t) :: t
  def new(msg, token, topic, id) do
    %Pigeon.APNS.Notification{
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
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"alert" => "push message"}},
        topic: nil
      }
  """
  @spec put_alert(t, String.t) :: t
  def put_alert(notification, alert), do: update_payload(notification, "alert", alert)

  @doc """
  Updates `"badge"` key in push payload.
  
  This is the badge number displayed on the application.

  ## Examples
  
      iex> Pigeon.APNS.Notification.put_badge(%Pigeon.APNS.Notification{}, 5)
      %Pigeon.APNS.Notification{
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"badge" => 5}},
        topic: nil
      }
  """
  @spec put_badge(t, integer) :: t
  def put_badge(notification, badge), do: update_payload(notification, "badge", badge)

  @doc """
  Updates `"sound"` key in push payload.

  Used for custom notification sounds. Value should
  be the name of the custom sound file in the application's binary.

  ## Examples
  
      iex> Pigeon.APNS.Notification.put_sound(%Pigeon.APNS.Notification{}, "custom.aiff")
      %Pigeon.APNS.Notification{
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"sound" => "custom.aiff"}},
        topic: nil
      }
  """
  @spec put_sound(t, String.t) :: t
  def put_sound(notification, sound), do: update_payload(notification, "sound", sound)

  @doc """
  Sets `"content-available"` flag in push payload.

  Used for silent notifications. When set, ensure `alert`, `badge`, and `sound` keys
  are not configured.

  ## Examples
  
      iex> Pigeon.APNS.Notification.put_content_available(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"content-available" => 1}},
        topic: nil
      }
  """
  @spec put_content_available(t) :: t
  def put_content_available(notification), do: update_payload(notification, "content-available", 1)

  @doc """
  Updates `"category"` key in push payload.

  ## Examples
  
      iex> Pigeon.APNS.Notification.put_category(%Pigeon.APNS.Notification{}, "category")
      %Pigeon.APNS.Notification{
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"category" => "category"}},
        topic: nil
      }
  """
  @spec put_category(t, String.t) :: t
  def put_category(notification, category), do: update_payload(notification, "category", category)

  @doc """
  Sets `"mutable-content"` flag in push payload.

  Used for notification service extensions (such as displaying custom media).

  ## Examples
  
      iex> Pigeon.APNS.Notification.put_mutable_content(%Pigeon.APNS.Notification{})
      %Pigeon.APNS.Notification{
        device_token: nil,
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"mutable-content" => 1}},
        topic: nil
      }
  """
  @spec put_mutable_content(t) :: t
  def put_mutable_content(notification), do: update_payload(notification, "mutable-content", 1)

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
        device_token: "device token",
        expiration: nil,
        id: nil,
        payload: %{"aps" => %{"alert" => "test message"}, "custom-key" => 1234},
        topic: nil
      }
  """
  @spec put_custom(t, String.t) :: t
  def put_custom(notification, data) do
    new_payload = Map.merge(notification.payload, data)
    %{notification | payload: new_payload}
  end
end

defmodule Pigeon.GCM.Notification do
  @moduledoc """
  Defines GCM notification struct and convenience constructor functions.
  """
  defstruct registration_id: nil, payload: %{}, message_id: nil, updated_registration_id: nil

  @type t :: %__MODULE__{
    message_id: String.t,
    payload: %{},
    registration_id: String.t | [String.t],
    updated_registration_id: String.t
  }

  @doc """
  Creates `GCM.Notification` struct with device registration IDs and optional
  notification and data payloads.

  ## Examples
  
      iex> Pigeon.GCM.Notification.new("reg ID")
      %Pigeon.GCM.Notification{
        message_id: nil,
        payload: %{},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.GCM.Notification.new("reg ID", %{"body" => "test message"})
      %Pigeon.GCM.Notification{
        message_id: nil,
        payload: %{"notification" => %{"body" => "test message"}},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.GCM.Notification.new("reg ID", %{"body" => "test message"}, %{"key" => "value"})
      %Pigeon.GCM.Notification{
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
    %Pigeon.GCM.Notification{registration_id: registration_ids}
    |> put_notification(notification)
    |> put_data(data)
  end

  @doc """
  Updates `"data"` key in push payload.

  ## Examples

      iex> Pigeon.GCM.Notification.put_data(%Pigeon.GCM.Notification{}, %{"key" => 1234})
      %Pigeon.GCM.Notification{
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

      iex> Pigeon.GCM.Notification.put_notification(%Pigeon.GCM.Notification{}, %{"body" => "message"})
      %Pigeon.GCM.Notification{
        message_id: nil,
        payload: %{"notification" => %{"body" => "message"}},
        registration_id: nil,
        updated_registration_id: nil
      }
  """
  def put_notification(n, notification), do: update_payload(n, "notification", notification)

  defp update_payload(notification, _key, value) when value == %{}, do: notification
  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)
    %{notification | payload: payload}
  end
end

defmodule Pigeon.ADM.Notification do
  @moduledoc """
  Defines Amazon ADM notification struct and convenience constructor functions.
  """
  defstruct registration_id: nil, payload: %{}, updated_registration_id: nil,
            consolidation_key: nil, expires_after: 604800, md5: nil

  @type t :: %__MODULE__{
    consolidation_key: String.t,
    expires_after: integer,
    md5: binary,
    payload: %{},
    registration_id: String.t,
    updated_registration_id: String.t
  }

  @doc """
  Creates `ADM.Notification` struct with device registration ID and optional data payload.

  ## Examples
  
      iex> Pigeon.ADM.Notification.new("reg ID")
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "1B2M2Y8AsgTpgAmY7PhCfg==",
        payload: %{"data" => %{}},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.ADM.Notification.new("reg ID", %{"message" => "your message"})
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "qzF+HgArKZjJrpfcTbiFxg==",
        payload: %{
          "data" => %{"message" => "your message"}
        },
        registration_id: "reg ID",
        updated_registration_id: nil
      }
  """
  def new(registration_id, data \\ %{})
  def new(registration_id, data) do
    %Pigeon.ADM.Notification{registration_id: registration_id}
    |> put_data(data)
  end

  @doc """
  Updates `"data"` key on push payload and calculates `md5` hash.

  ## Examples
  
      iex> n = %Pigeon.ADM.Notification{}
      iex> Pigeon.ADM.Notification.put_data(n, %{"message" => "your message"})
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "qzF+HgArKZjJrpfcTbiFxg==",
        payload: %{
          "data" => %{"message" => "your message"}
        },
        registration_id: nil,
        updated_registration_id: nil
      }
  """
  def put_data(n, data) do
    n
    |> update_payload("data", ensure_strings(data))
    |> calculate_md5
  end

  defp update_payload(notification, key, value) when value == %{} and key != "data",
    do: notification
  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)
    %{notification | payload: payload}
  end

  @doc """
  ADM requires that "data" keys and values are all strings.
  """
  def ensure_strings(data) do
    data
    |> Enum.map(fn {key, value} -> {"#{key}", "#{value}"} end)
    |> Enum.into(%{})
  end

  @doc """
  Calculates md5 hash of notification data payload.
  """
  def calculate_md5(%{payload: %{"data" => data}} = notification) when is_map(data) do
    concat =
      data
      |> Map.keys
      |> Enum.sort
      |> Enum.map(fn key -> "#{key}:#{data[key]}" end)
      |> Enum.join(",")

    md5 = :md5 |> :crypto.hash(concat) |> Base.encode64

    %{notification | md5: md5}
  end
  def calculate_md5(notification), do: notification
end
