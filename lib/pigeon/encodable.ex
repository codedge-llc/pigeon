defprotocol Pigeon.Encodable do
  @spec binary_payload(any) :: binary | :error
  def binary_payload(notif)
end
