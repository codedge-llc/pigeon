defmodule Pigeon.Http2.Client do
  @moduledoc ~S"""
  HTTP2 client adapter behaviour

  Uses `Kadabra` by default, but any client can be used
  with the appropriate adapter.

  ## Writing a Custom Client Adapter

  Adapters must implement five callbacks.

  # TODO: Finish this guide

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
                                              status: status,
                                              headers: headers,
                                              body: body}}, _state) do
          {:ok, %Pigeon.Http2.Stream{
              id: id,
              status: status,
              headers: headers,
              body: body
          }}
        end
        def handle_end_stream(msg, _state), do: msg
      end
      end
  """

  @doc ~S"""
  Default http2 client to use.
  
  When not configured, defaults to `Pigeon.Http2.Client.Kadabra`

  ## Examples

      iex> Pigeon.Http2.Client.default
      Pigeon.Http2.Client.Kadabra
  """
  def default do
    Application.get_env(:pigeon, :http2_client, Pigeon.Http2.Client.Kadabra)
  end

  @callback start() :: no_return

  @callback connect(uri :: charlist, scheme :: :https, options :: Keyword.t)
    :: {:ok, term}
     | {:error, term}

  @callback send_ping(pid) :: :ok

  @callback send_request(pid, headers :: Keyword.t, data :: String.t) :: :ok

  @callback handle_end_stream(msg :: term, state :: term)
    :: {:ok, %Pigeon.Http2.Stream{}}
     | term
end
