defmodule Pigeon.Metadata do
  @moduledoc false

  @type t :: %__MODULE__{
          on_response: Pigeon.on_response()
        }

  defstruct on_response: nil
end
