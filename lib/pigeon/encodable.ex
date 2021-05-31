defprotocol Pigeon.Encodable do
  @moduledoc false

  @spec binary_payload(any) :: binary | :error
  def binary_payload(notif)
end
