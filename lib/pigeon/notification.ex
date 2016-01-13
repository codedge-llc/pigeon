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

  def hexstr_to_bin(s) do
    hexstr_to_bin(s, [])
  end

  def hexstr_to_bin([], acc) do
    :erlang.list_to_binary(:lists.reverse(acc))
  end

  def hexstr_to_bin([x, y|t], acc) do
    {:ok, [v], []} = :io_lib.fread('~16u', [x, y])
    hexstr_to_bin(t, [v|acc])
  end
end

defmodule Pigeon.APNS.Notification do

  def new(msg, token) do
    new(msg, token, %{}, %{})
  end
  def new(msg, token, options) do
    new(msg, token, options, %{})
  end
  def new(msg, token, options, custom) do
    new_options = Map.put(options, :alert, msg)
    b_payload = Map.merge(%{aps: new_options}, custom)
      |> Pigeon.Notification.json_payload

    payload_len = :erlang.size(b_payload)

    b_token = to_char_list(token) |> Pigeon.Notification.hexstr_to_bin
    b_token_length = :erlang.byte_size(b_token)

    id = 15
    {mseconds, seconds, _} = :erlang.timestamp()
    expiry = mseconds * 1000000 + seconds + 3600*1

    push_packet(id, expiry, b_token_length, b_token, payload_len, b_payload)
  end

  defp push_packet(id, expiry, b_token_length, b_token, payload_len, b_payload) do
    << 1 >> 
    <> << id :: size(32) >>
    <> << expiry :: size(32) >>
    <> << b_token_length :: size(16) >> 
    <> b_token
    <> << payload_len :: size(16) >>
    <> b_payload
  end
end

defmodule Pigeon.GCM.Notification do
  def new(data, token) do
    Pigeon.Notification.json_payload(%{to: token, data: data})
  end
end
