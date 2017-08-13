if Code.ensure_loaded?(Kadabra) do
defmodule Pigeon.Http2.Client.Kadabra do
  @moduledoc false

  @behaviour Pigeon.Http2.Client

  def start do
    Application.ensure_all_started(:kadabra)
  end

  def connect(uri, scheme, opts) do
    Kadabra.open(uri, scheme, opts)
  end

  def send_request(pid, headers, data) do
    Kadabra.request(pid, headers, data)
  end

  def send_ping(pid) do
    Kadabra.ping(pid)
  end

  def handle_end_stream({:end_stream, %{id: id,
                                        headers: headers,
                                        body: body}}, _state) do
    {:ok, %Pigeon.Http2.Stream{id: id, headers: headers, body: body}}
  end
  def handle_end_stream(msg, _state), do: msg
end
end
