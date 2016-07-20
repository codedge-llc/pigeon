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

  def new(msg, token, topic \\ nil) do
    %Pigeon.APNS.Notification{
      device_token: token,
      topic: topic,
      payload: %{"aps" => %{"alert" => msg}}
    }
  end

  def new(msg, token, topic, id) do
    %Pigeon.APNS.Notification{
      device_token: token,
      topic: topic,
      payload: %{"aps" => %{"alert" => msg}},
      id: id
    }
  end

  def put_alert(notification, alert), do: update_payload(notification, "alert", alert)

  def put_badge(notification, badge), do: update_payload(notification, "badge", badge)

  def put_sound(notification, sound), do: update_payload(notification, "sound", sound)

  def put_content_available(notification), do: update_payload(notification, "content-available", 1)

  def put_category(notification, category), do: update_payload(notification, "category", category)

  defp update_payload(notification, key, value) do
    new_aps =
      notification.payload
      |> Map.get("aps")
      |> Map.put(key, value)
    new_payload = notification.payload |> Map.put("aps", new_aps)
    %{notification | payload: new_payload}
  end

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

  def new(registration_ids, notification \\ %{}, data \\ %{})
  def new(registration_ids, notification, data) do
    %Pigeon.GCM.Notification{registration_id: registration_ids}
    |> put_notification(notification)
    |> put_data(data)
  end

  def put_data(n, data), do: update_payload(n, "data", data)

  def put_notification(n, notification), do: update_payload(n, "notification", notification)

  defp update_payload(notification, _key, value) when value == %{}, do: notification
  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)
    %{notification | payload: payload}
  end
end
