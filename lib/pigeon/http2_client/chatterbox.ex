defmodule Pigeon.Http2.Client.Chatterbox do
  @behaviour Pigeon.Http2Client

  def connect(uri, scheme, opts) do
    :h2_client.start_link(scheme, uri, opts)
  end

  def send_request(pid, headers, data) do
    :h2_client.send_request(pid, headers, data)
  end

  def send_ping(pid) do
    :h2_client.send_ping(pid)
  end

  def handle_end_stream({:END_STREAM, stream}, state) do
    {:ok, {headers, body}} = :h2_client.get_response(state.apns_socket, stream)
    {:ok, %Pigeon.Http2.Stream{id: stream, headers: headers, body: body}}
  end
  def handle_end_stream(msg) do
    IO.inspect(msg)
  end
end
