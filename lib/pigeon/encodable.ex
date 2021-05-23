defprotocol Pigeon.Encodable do
  @spec binary_payload(any) :: binary | :error
  def binary_payload(notif)
end

defimpl Pigeon.Encodable, for: Atom do
  def binary_payload(atom), do: atom
end
