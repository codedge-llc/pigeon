defmodule Pigeon.Http2.Client do
  @moduledoc ~S"""
  HTTP2 client adapter behaviour

  Uses `Kadabra` by default, but any client can be used
  with the appropriate adapter.

  ## Writing a Custom Client Adapter

  `Pigeon.Worker` relies on all of the callbacks for important
  connection functionality.

  * `start/0`
    * Starts the client application when `Pigeon` starts.
  * `connect/3`
    * Opens a socket connection. Must return `{:ok, pid}` or
      `{:error, reason}`. If an error, `Pigeon.Worker` will retry
      the callback two more times.
  * `send_ping/1`
    * Sends an HTTP2 ping. `Pigeon.Worker` periodically sends pings to keep
      the connection alive. Client adapters must support pings for
      APNS-configured workers, though it is not necessary if only using FCM.
  * `send_request/3`
    * Makes an HTTP2 request. `Pigeon.Worker` does not handle synchronous
      requests and will ignore the result of this callback. If the client
      is synchronous, the adapter will need to explicitly send a message
      back to the `Pigeon.Worker`.
  * `handle_end_stream/2`
    * All incoming messages on the worker are passed through this callback.
      Must return `{:ok, %Pigeon.Http2.Stream{...}}` if it is a valid
      `END_STREAM` response for the adapter. All other messages are ignored.

  **Example implementation for Kadabra**

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

  ## Using Your Client Adapter

  Once implemented, specify it in your config. Pigeon will use it for all
  HTTP2 connections.

      config :pigeon, http2_client: Pigeon.YourCustomAdapter
  """

  @type uri :: charlist

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

  @callback connect(uri :: uri, scheme :: :https, options :: Keyword.t()) ::
              {:ok, pid} | {:error, any}

  @callback send_ping(pid) :: :ok

  @callback send_request(pid, headers :: Keyword.t(), data :: String.t()) :: :ok

  @callback handle_end_stream(msg :: term, state :: term) ::
              {:ok, %Pigeon.Http2.Stream{}}
              | any
end
