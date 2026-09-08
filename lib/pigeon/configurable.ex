defprotocol Pigeon.Configurable do
  @moduledoc false

  @spec connect(any) :: {:ok, Mint.HTTP2.t()} | {:error, Exception.t()}
  def connect(config)

  def push_headers(config, notification, opts)

  def push_payload(config, notification, opts)

  @doc ~S"""
  Interval between keepalive pings in milliseconds, or `nil` for none.

  ## Examples

      iex> ping_period(%Pigeon.APNS.Config{ping_period: 2})
      2

      iex> ping_period(%Pigeon.FCM.Config{})
      600_000
  """
  @spec ping_period(any) :: pos_integer | nil
  def ping_period(config)

  def close(config)

  @spec validate!(any) :: :ok | no_return
  def validate!(config)
end
