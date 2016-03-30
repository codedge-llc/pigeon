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
  defstruct registration_id: nil, data: nil, message_id: nil, updated_registration_id: nil

  def new(data, registration_ids) when is_list(registration_ids) do
    %Pigeon.GCM.Notification{registration_id: registration_ids, data: data}
  end

  def new(data, registration_id) do
    %Pigeon.GCM.Notification{registration_id: registration_id, data: data}
  end
end
