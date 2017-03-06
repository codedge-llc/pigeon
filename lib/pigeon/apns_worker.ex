defmodule Pigeon.APNSWorker do
  @moduledoc """
    Handles all APNS request and response parsing over an HTTP2 connection.
  """
  use Pigeon.GenericH2Worker
  require Logger

  def host(config) do
    case config[:mode] do
      :dev -> config[:development_endpoint]
      :prod -> config[:production_endpoint]
    end
  end

  def port(config) do
    case config[:use_2197] do
      true -> 2197
      _ -> config[:port]
    end
  end

  def socket_options(config) do
    cert = get_opt(config, :cert, :certfile)
    key = get_opt(config, :key, :keyfile)
    cond do
      cert && key ->
        options =
          [cert,
          key,
          {:password, ''}]
        {:ok, options}
      true ->
        {:error, :invalid_config}
    end
  end

  defp get_opt(config, key_1, key_2) do
    cond do
      config[key_1] -> {key_1, config[key_1]}
      config[key_2] -> {key_2, config[key_2]}
      true -> nil
    end
  end

  def req_headers(_config, notification) do
    []
    |> put_apns_id(notification)
    |> put_apns_topic(notification)
  end

  defp put_apns_id(headers, notification) do
    case notification.id do
      nil -> headers
      id -> headers ++ [{"apns-id", id}]
    end
  end

  defp put_apns_topic(headers, notification) do
    case notification.topic do
      nil   -> headers
      topic -> headers ++ [{"apns-topic", topic}]
    end
  end

  def req_path(notification) do
    "/3/device/#{notification.device_token}"
  end

  defp parse_error(_notification, _headers, body) do
    {:ok, response} = Poison.decode(body)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  defp parse_response(notification, headers, _body) do
    case List.keyfind(headers, "apns-id", 1) do
      nil -> notification
      apns_id ->
        %{notification | id: apns_id}
    end
  end

  def error_msg(_code, error) do
    case error do
      # 400
      :bad_collapse_id ->
        "The collapse identifier exceeds the maximum allowed size"
      :bad_device_token ->
        """
        The specified device token was bad. Verify that the request contains a valid token and
        that the token matches the environment.
        """
      :bad_expiration_date ->
        "The apns-expiration value is bad."
      :bad_message_id ->
        "The apns-id value is bad."
      :bad_priority ->
        "The apns-priority value is bad."
      :bad_topic ->
        "The apns-topic was invalid."
      :device_token_not_for_topic ->
        "The device token does not match the specified topic."
      :duplicate_headers ->
        "One or more headers were repeated."
      :idle_timeout ->
        "Idle time out."
      :missing_device_token ->
        """
        The device token is not specified in the request :path. Verify that the :path header
        contains the device token.
        """
      :missing_topic ->
          """
          The apns-topic header of the request was not specified and was required. The apns-topic
          header is mandatory when the client is connected using a certificate that supports
          multiple topics.
          """
      :payload_empty ->
        "The message payload was empty."
      :topic_disallowed ->
        "Pushing to this topic is not allowed."

      # 403
      :bad_certificate ->
        "The certificate was bad."
      :bad_certificate_environment ->
        "The client certificate was for the wrong environment."
      :expired_provider_token ->
        "The provider token is stale and a new token should be generated."
      :forbidden ->
        "The specified action is not allowed."
      :invalid_provider_token ->
        "The provider token is not valid or the token signature could not be verified."
      :missing_provider_token ->
        """
        No provider certificate was used to connect to APNs and Authorization header was missing
        or no provider token was specified."
        """

      # 404
      :bad_path ->
        "The request contained a bad :path value."

      # 405
      :method_not_allowed ->
        "The specified :method was not POST."

      # 410
      :unregistered ->
        "The device token is inactive for the specified topic."

      # 413
      :payload_too_large ->
        "The message payload was too large. The maximum payload size is 4096 bytes."
      # 429
      :too_many_provider_token_updates ->
        "The provider token is being updated too often."
      :too_many_requests ->
        "Too many requests were made consecutively to the same device token."

      # 500
      :internal_server_error ->
        "An internal server error occurred."

      # 503
      :service_unavailable ->
        "The service is unavailable."
      :shutdown ->
        "The server is shutting down."

      # Misc
      :timeout ->
        "The SSL connection timed out."
      _ ->
        ""
    end
  end
end
