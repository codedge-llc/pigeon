defprotocol Pigeon.Configurable do
  @type sock :: {:sslsocket, any, pid | {any, any}}

  @spec worker_name(any) :: atom | nil
  def worker_name(config)

  @spec connect(any) :: {:ok, sock} | {:error, String.t}
  def connect(config)

  def push_headers(config, notification, opts)

  def push_payload(config, notification, opts)

  def handle_end_stream(config, stream, notification, on_response)

  @spec ping_period(any) :: pos_integer
  def ping_period(config)

  @spec reconnect?(any) :: boolean
  def reconnect?(config)

  def close(config)
end
