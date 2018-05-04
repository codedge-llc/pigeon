defmodule Pigeon.ConfigError do
  @moduledoc """
  This error represents configuration errors: for example, configuring
  `:token key` and `:cert` for APNS.
  """

  defexception [:message]

  @type t :: %__MODULE__{message: binary}
end
