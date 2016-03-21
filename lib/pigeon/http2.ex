defmodule Pigeon.HTTP2 do
  require Logger

  defp apns_production_api_uri, do: 'api.push.apple.com'
  defp apns_development_api_uri, do: 'api.development.push.apple.com'

  def connect(mode, cert, key) do
    uri = push_uri(mode)
    options = [{:certfile, cert}, 
               {:keyfile, key},
               {:password, ''},
               {:packet, 0},
               {:reuseaddr, true},
               {:active, true},
               :binary]
    :ssl.start
    case :ssl.connect(uri, 443, options) do
      {:ok, ssl_socket} ->
        {:ok, ssl_socket}
      {:error, reason} ->
        IO.inspect {:error, reason}
    end
  end

  def push_uri(mode) do
    case mode do
      :dev ->
        apns_development_api_uri
      :prod ->
        apns_production_api_uri
    end
  end

  def send_connection_preface(socket) do
    :ssl.send(socket, connection_preface)
    do_receive_once(socket)
  end

  def status_code(<<1::1, status::7, rest::binary>>) do
    case status do
      8 -> 200
      9 -> 204
      10 -> 206
      11 -> 304
      12 -> 400
      13 -> 404
      14 -> 500
    end
  end

	def establish_connection(socket) do
    {:ok, data} = send_settings(socket)
    if data = build_frame(0x04, 0x01, 0, <<>>) do
      send_ack(socket)
    else
      {:error, "Can't establish a connection."}
    end
  end

  defp send_settings sock do
    :ssl.send(sock, settings_frame)
    do_receive_once sock
  end

  defp send_ack sock do
    :ssl.send(sock, settings_ack_frame)
  end

  defp send_goaway sock do
    :ssl.send sock, goaway_frame
    :ssl.close sock
  end

  defp do_receive_once socket do
    receive do
      {:ssl, socket, bin} ->
        frame = bin |> parse_frame
        parse_frame_type(frame, bin)
      {:ssl_closed, socket} ->
        {:error, "closed."}
      {:ssl_error, socket, reason} ->
        {:error, reason}
    after
      5000 ->
        {:error, "timeout."}
    end
  end

  def wait_response socket do
    receive do
      {:ssl, socket, bin} ->
        case wait_payload(socket) do
          {:ok, payload} ->
            {:ok, bin, payload}
          error ->
            error
        end
      {:ssl_closed, socket} ->
        {:error, "closed."}
      {:ssl_error, socket, reason} ->
        {:error, reason}
    after
      5000 ->
        {:error, "timeout."}
    end
  end

  defp wait_payload socket do
    receive do
      {:ssl, socket, bin} ->
        {:ok, bin}
      {:ssl_closed, socket} ->
        {:error, "closed."}
      {:ssl_error, socket, reason} ->
        {:error, reason}
    after
      5000 ->
        {:error, "timeout."}
    end
  end

  def parse_frame_type(frame, bin) do
    case frame[:frame_type] do
      0x0 ->
        Logger.debug "Got data frame..."
        {:ok, frame}
      0x1 ->
        Logger.debug "Got header frame..."
        IO.inspect frame
        {:ok, frame}
      0x2 ->
        Logger.debug "Got priority frame..."
        {:ok, frame}
      0x3 ->
        <<error_code::32, _::binary>> = frame[:payload]
        {:error, "RST_STREAM error_code:#{error_code}"}
      0x7 ->
        <<error_code::32, _::binary>> = frame[:payload]
        {:error, "GOAWAY error_code:#{error_code}"}
      _ ->
        {:ok, bin}
    end
  end

  def parse_frame(<<payload_size::24, frame_type::8, flags::8, 0::1, stream_id::31, payload::binary>>) do
    %{
      payload_size: payload_size,
      frame_type: frame_type,
      flags: flags,
      stream_id: stream_id,
      payload: payload
    }
  end

  def parse_frame(bin) do
    bin 
  end

	def connection_preface, do: "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  def settings_frame, do: build_frame(0x4, 0, 0, <<>>)
  def settings_ack_frame, do: build_frame(0x04, 0x01, 0, <<>>)
  def ack_frame, do: build_frame(0x01, 0, 0, <<>>)
  def ping_frame, do: build_frame(0x6, 0x0, 0, <<1::64>>)

  def goaway_frame do
    no_error = 0
    build_frame(0x7, 0x0, 0, <<0::31, 1::1, no_error::32>>)
  end

  def push_header_frame(stream, mode, device_token, topic, payload) do
    uri = push_uri(mode) |> to_string
    end_headers = 0x4
    payload_size = "#{byte_size(payload)}"
    build_frame(0x1, end_headers, stream, <<
      <<1::1, 0::1, 0::1, 0::1, 0::1, 0::1, 1::1, 1::1>>, # method POST
      <<1::1, 0::1, 0::1, 0::1, 0::1, 1::1, 1::1, 1::1>>, # scheme HTTPS
      <<0::1, 0::1, 0::1, 1::1, 0::4, 0::1, byte_size(":path")::7, ":path", 0::1, byte_size("/3/device/#{device_token}")::7, "/3/device/#{device_token}">>,
      <<0::1, 0::1, 0::1, 1::1, 0::4, 0::1, byte_size("host")::7, "host", 0::1, byte_size(uri)::7, "#{uri}">>,
      <<0::1, 0::1, 0::1, 1::1, 0::4, 0::1, byte_size("apns-topic")::7, "apns-topic", 0::1, byte_size(topic)::7, "#{topic}">>,
      <<0::1, 0::1, 0::1, 1::1, 0::4, 0::1, byte_size("content-length")::7, "content-length", 0::1, byte_size(payload_size)::7, "#{payload_size}">>
    >>)
  end

  def push_data_frame(stream, payload) do
    build_frame(0x0, 0x1, stream, payload)
  end

  def build_frame(frame_type, flags, stream_id, payload) do
    header = <<byte_size(payload)::24, frame_type::8, flags::8, 0::1, stream_id::31>>
    <<header::binary, payload::binary>>
  end
end
