defmodule Pigeon.Http2.Client do
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
