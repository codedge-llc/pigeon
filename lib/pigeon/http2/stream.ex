defmodule Pigeon.Http2.Stream do
  @moduledoc false

  defstruct id: nil, status: nil, headers: nil, body: nil, error: nil

  @type t :: %__MODULE__{
          id: pos_integer,
          headers: [{binary, binary}, ...],
          body: binary,
          error: term
        }
end
