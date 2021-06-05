defprotocol Pigeon.Configurable do
  @moduledoc false

  @type sock :: {:sslsocket, any, pid | {any, any}}

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(config)

  def push_headers(config, notification, opts)

  def push_payload(config, notification, opts)

  def handle_end_stream(config, stream, notification, on_response)

  @doc ~S"""
  Schedules connection ping if necessary.

  ## Examples

      iex> schedule_ping(%Pigeon.APNS.Config{ping_period: 2})
      iex> receive do
      ...>   :ping -> "Got ping!"
      ...> after
      ...>   5000 -> "No ping received."
      ...> end
      "Got ping!"

      iex> schedule_ping(%Pigeon.FCM.Config{})
      iex> receive do
      ...>   :ping -> "Got ping!"
      ...> after
      ...>   5000 -> "No ping received."
      ...> end
      "No ping received."
  """
  @spec schedule_ping(any) :: no_return
  def schedule_ping(config)

  def close(config)

  @spec validate!(any) :: :ok | no_return
  def validate!(config)
end
