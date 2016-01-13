defmodule Pigeon.APNS.Connection do
  import Logger
  require :crypto

  def apple_production_gateway_uri, do: 'gateway.push.apple.com'
  def apple_development_gateway_uri, do: 'gateway.sandbox.push.apple.com'

  def apple_production_feedback_uri, do: 'feedback.push.apple.com'
  def apple_development_feedback_uri, do: 'feedback.sandbox.push.apple.com'

  def new(mode, cert, key) do
    :ssl.start()
    options = [{:certfile, cert}, {:keyfile, key}, {:reuse_sessions, false}, {:mode, :binary}]
    open(mode, options)
  end

  def new_feedback(mode, cert, key) do
    :ssl.start()
    options = [{:certfile, cert}, {:keyfile, key}, {:reuse_sessions, false}, {:mode, :binary}]
    open_feedback(mode, options)
  end

  def new(mode, cert, key, passphrase) do
    :ssl.start()
    :application.start(:inets)
    options = [{:certfile, cert}, {:keyfile, key}, {:password, passphrase}, 
      {:mode, :binary}, {:active, false}]
    open(mode, options)
  end

  defp open(mode, options) do
    case mode do
      :dev ->
        connect(apple_development_gateway_uri, 2195, options)
      :prod ->
        connect(apple_production_gateway_uri, 2195, options)
      _ ->
        {:error, "Mode is invalid."}
    end
  end

  defp open_feedback(mode, options) do
    case mode do
      :dev ->
        connect(apple_development_feedback_uri, 2196, options)
      :prod ->
        connect(apple_production_feedback_uri, 2196, options)
      _ ->
        {:error, "Mode is invalid."}
    end
  end

  defp connect(uri, port, options) do
    case :ssl.connect(uri, port, options, 1000) do
      {:ok, ssl_socket} ->
        ssl_socket
      {:error, reason} ->
        IO.inspect {:error, reason}
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
