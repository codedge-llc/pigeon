defmodule Pigeon do
  @moduledoc """
  Pigeon is a wrapper for sending iOS push notifications.
  """

  def apple_production_gateway_uri, do: 'gateway.push.apple.com'
  def apple_development_gateway_uri, do: 'gateway.sandbox.push.apple.com'
  
  def push(notification, uri, cert, key) do
    case Pigeon.Connection.open(uri, cert, key) do
      {:ok, ssl_socket} ->
        push(notification, ssl_socket)
      {:error, reason} ->
        IO.inspect {:error, reason}
    end
  end

  def push(packet, ssl_socket) do
    :ssl.send(ssl_socket, packet)
    :ssl.close(ssl_socket)
  end
end

defmodule Pigeon.Notification do
  def new(msg, token) do
    b_payload = encode_payload(msg, "default")
    payload_len = :erlang.size(b_payload)

    b_token = hexstr_to_bin(token)
    b_token_length = :erlang.byte_size(b_token)

    some_id = 1
    {mseconds, seconds, _} = :erlang.timestamp()
    expiry = mseconds * 1000000 + seconds + 3600*1

    push_packet(some_id, expiry, b_token_length, b_token, payload_len, b_payload)
  end

  def push_packet(id, expiry, b_token_length, b_token, payload_len, b_payload) do
    << 1 >> 
    <> << id :: size(32) >>
    <> << expiry :: size(32) >>
    <> << b_token_length :: size(16) >> 
    <> b_token
    <> << payload_len :: size(16) >>
    <> b_payload
  end

  def encode_payload(msg) do
    "{\"aps\": {\"alert\": \"" <> msg <> "\"}}"
  end
  def encode_payload(msg, sound) do
    "{\"aps\": {\"alert\": \"" <> msg <> "\", \"sound\": \"" <> sound <> "\"}}"
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

defmodule Pigeon.Connection do
  def open(uri, cert, key) do
    :ssl.start()
    options = [{:certfile, cert}, {:keyfile, key}, {:mode, :binary}]
    :ssl.connect(uri, 2195, options, 1000)
  end
end
