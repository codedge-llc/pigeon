defmodule Pigeon.Notification do
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

  @spec new(String.t, String.t, String.t) :: %{device_token: String.t, topic: String.t, payload: String.t}
  def new(msg, token, topic) do
    new(msg, token, topic, %{}, %{})
  end

  @spec new(String.t, String.t, String.t, %{}) :: %{device_token: String.t, topic: String.t, payload: String.t}
  def new(msg, token, topic, options) do
    new(msg, token, topic, options, %{})
  end

  @spec new(String.t, String.t, String.t, %{}, %{}) :: %{device_token: String.t, topic: String.t, payload: String.t}
  def new(msg, token, topic, options, custom) do
    new_options = Map.put(options, :alert, msg)
    payload = Map.merge(%{aps: new_options}, custom)
      |> Pigeon.Notification.json_payload

    %{device_token: token, topic: topic, payload: payload}
  end
end

defmodule Pigeon.GCM.Notification do
  @spec new(String.t, String.t) :: %{to: String.t, data: String.t}
  def new(data, token) do
    Pigeon.Notification.json_payload(%{to: token, data: data})
  end
end
