defprotocol Pigeon.Configurable do
  @type sock :: {:sslsocket, any, pid | {any, any}}

  @doc ~S"""
  Returns worker name for config.

  ## Examples

      iex> worker_name(%Pigeon.APNS.Config{name: :test})
      :test

      iex> worker_name(%Pigeon.FCM.Config{name: :another})
      :another
  """
  @spec worker_name(any) :: atom | nil
  def worker_name(config)

  @spec connect(any) :: {:ok, sock} | {:error, String.t}
  def connect(config)

  def push_headers(config, notification, opts)

  def push_payload(config, notification, opts)

  def handle_end_stream(config, stream, notification, on_response)

  @doc ~S"""
  Returns ping_period for config.

  ## Examples

      iex> ping_period(%Pigeon.APNS.Config{ping_period: 600})
      600

      iex> ping_period(%Pigeon.FCM.Config{ping_period: 300})
      300
  """
  @spec ping_period(any) :: pos_integer
  def ping_period(config)

  @doc ~S"""
  Returns whether connection should reconnect if dropped.

  ## Examples

      iex> reconnect?(%Pigeon.APNS.Config{reconnect: true})
      true

      iex> reconnect?(%Pigeon.FCM.Config{}) # always false
      false
  """
  @spec reconnect?(any) :: boolean
  def reconnect?(config)

  def close(config)
end
