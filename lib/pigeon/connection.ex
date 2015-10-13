defmodule Pigeon.APNS.Connection do
  import Logger
  defstruct certfile: nil, keyfile: nil, password: nil, ssl_socket: nil

  def apple_production_gateway_uri, do: 'gateway.push.apple.com'
  def apple_development_gateway_uri, do: 'gateway.sandbox.push.apple.com'

  def new(mode, cert, key) do
    :ssl.start()
    options = [{:certfile, cert}, {:keyfile, key}, {:mode, :binary}, {:active, false}]
    connection = %Pigeon.APNS.Connection{certfile: cert, keyfile: key}
    open(mode, options, connection)
  end

  def new(mode, cert, key, passphrase) do
    :ssl.start()
    options = [{:certfile, cert}, {:keyfile, key}, {:password, passphrase}, 
      {:mode, :binary}, {:active, false}]
    connection = %Pigeon.APNS.Connection{certfile: cert, keyfile: key, password: passphrase}
    open(mode, options, connection)
  end

  defp open(mode, options, connection) do
    case mode do
      :dev ->
        connect(apple_development_gateway_uri, options, connection)
      :prod ->
        connect(apple_production_gateway_uri, options, connection)
      _ ->
        {:error, "Mode is invalid."}
    end
  end

  defp connect(uri, options, connection) do
    case :ssl.connect(uri, 2195, options, 1000) do
      {:ok, ssl_socket} ->
        %{connection | ssl_socket: ssl_socket}
      {:error, reason} ->
        IO.inspect {:error, reason}
    end
  end

  def listen(connection) do
    response = :ssl.recv(connection, 0)
    case response do
      {:ok, data} ->
        parse_response(data)
      {:error, error} ->
        Logger.info "Error: #{error}"
    end
  end
  
  def parse_response(<<8, status, identifier :: binary>>) do
    {status, identifier}
  end

  def parse_response(<< _response :: binary >>) do
    {:error, :invalid_response}
  end

  def parse_status(status) do
    case status do
      0 ->
        :no_errors
      1 ->
        :processing_error
      2 ->
        :missing_device_token
      3 ->
        :missing_topic
      4 ->
        :missing_payload
      5 ->
        :invalid_token_size
      6 ->
        :invalid_topic_size
      7 ->
        :invalid_payload_size
      8 ->
        :invalid_token
      10 ->
        :shutdown
      255 ->
        :none
      _ ->
        :unknown
    end
  end

  def close(connection) do
    :ssl.close(connection)
  end
end
