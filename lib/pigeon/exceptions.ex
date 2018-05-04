defmodule Pigeon.ConfigError do
  @moduledoc """
  This error represents configuration errors: for example, configuring
  both `:cert` and `:jwt_key` for APNS.
  """

  defexception [:message]

  @type t :: %__MODULE__{message: binary}
end
