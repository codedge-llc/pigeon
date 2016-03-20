defmodule Pigeon.Notification do
  import Logger

  def json_payload(payload) do
    response = Poison.encode(payload)
    case response do
    {:ok, result} ->
      result
    {:error, error} ->
      Logger.error error
    end
  end

  #def hexstr_to_bin(s) do
  #  hexstr_to_bin(s, [])
  #end

  #def hexstr_to_bin([], acc) do
  #  :erlang.list_to_binary(:lists.reverse(acc))
  #end

  #def hexstr_to_bin([x, y|t], acc) do
  #  {:ok, [v], []} = :io_lib.fread('~16u', [x, y])
  #  hexstr_to_bin(t, [v|acc])
  #end
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
  def new(data, token) do
    Pigeon.Notification.json_payload(%{to: token, data: data})
  end
end
