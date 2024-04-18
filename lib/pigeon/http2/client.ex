defmodule Pigeon.Http2.Client do
  @moduledoc false

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

  @callback send_ping(pid, <<_::64>>) :: :ok

  @callback send_request(
              pid,
              headers :: [{binary, binary}, ...],
              data :: String.t()
            ) ::
              :ok

  @callback handle_end_stream(msg :: term, state :: term) ::
              {:ok, %Pigeon.Http2.Stream{}}
              | any
end
