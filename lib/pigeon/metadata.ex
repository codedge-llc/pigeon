defmodule Pigeon.Metadata do
  defstruct on_response: nil

  def on_response?(notification) do
    notification.__meta__.on_response != nil
  end
end
