defprotocol Pigeon.Configurable do
  @type sock :: {:sslsocket, any, pid | {any, any}}

  @spec default_name(any) :: atom
  def default_name(config)

  @spec worker_name(any) :: atom | nil
  def worker_name(config)

  @spec connect(any) :: {:ok, sock} | {:error, String.t}
  def connect(config)

  def push_headers(config, notification)

  def push_payload(config, notification)

  def handle_end_stream(config, stream, notification, on_response)

  @spec ping_period(any) :: pos_integer
  def ping_period(config)

  @spec reconnect(any) :: boolean
  def reconnect(config)

  def close(config)
end
