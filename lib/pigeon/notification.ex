defmodule Pigeon.Notification do
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
  defstruct device_token: nil, payload: %{"aps" => %{}}, expiration: nil, topic: nil

  @spec new(String.t, String.t, String.t) :: %{msg: String.t, device_token: String.t, topic: String.t}
  def new(msg, token, topic) do
    %Pigeon.APNS.Notification{device_token: token, topic: topic, payload: %{"aps" => %{"alert" => msg}}}
  end

  def put_alert(notification, alert), do: update_payload(notification, "alert", alert)

  def put_badge(notification, badge), do: update_payload(notification, "badge", badge)

  def put_sound(notification, sound), do: update_payload(notification, "sound", sound)

  def put_content_available(notification), do: update_payload(notification, "content-available", 1)

  def put_category(notification, category), do: update_payload(notification, "category", category)

  defp update_payload(notification, key, value) do
    new_aps = Map.get(notification.payload, "aps") |> Map.put(key, value)
    new_payload = notification.payload |> Map.put("aps", new_aps)
    %{notification | payload: new_payload}
  end
  
  def put_custom(notification, data) do
    new_payload = Map.merge(notification.payload, data)
    %{notification | payload: new_payload}
  end
end

defmodule Pigeon.GCM.Notification do
  defstruct registration_id: nil, data: nil, message_id: nil, updated_registration_id: nil

  def new(data, registration_ids) when is_list(registration_ids) do
    %Pigeon.GCM.Notification{registration_id: registration_ids, data: data}
  end

  def new(data, registration_id) do
    %Pigeon.GCM.Notification{registration_id: registration_id, data: data}
  end
end
