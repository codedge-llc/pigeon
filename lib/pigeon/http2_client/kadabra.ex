defmodule Pigeon.Http2.Client.Kadabra do
  @behaviour Pigeon.Http2.Client

  def connect(uri, scheme, opts) do
    if Code.ensure_loaded?(Kadabra) do
      Kadabra.open(uri, scheme, opts)
    end
  end

  def send_request(pid, headers, data) do
    if Code.ensure_loaded?(Kadabra) do
      Kadabra.request(pid, headers, data)
    end
  end

  def send_ping(pid) do
    if Code.ensure_loaded?(Kadabra) do
      Kadabra.ping(pid)
    end
  end

  def handle_end_stream({:end_stream, %{id: id,
                                        headers: headers,
                                        body: body}}, _state) do

    {:ok, %Pigeon.Http2.Stream{id: id, headers: headers, body: body}}
  end
  def handle_end_stream(msg, _state), do: msg
end
