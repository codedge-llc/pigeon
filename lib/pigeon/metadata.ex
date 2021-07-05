defmodule Pigeon.Metadata do
  @moduledoc """
  Internal push notification metadata.
  """

  @type t :: %__MODULE__{
          on_response: Pigeon.on_response() | nil
        }

  defstruct on_response: nil
end
