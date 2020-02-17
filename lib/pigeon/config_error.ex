defmodule Pigeon.ConfigError do
  defexception reason: nil, config: nil

  @impl true
  def message(%{config: config, reason: reason}) do
    """
    #{reason}

    The following configuration was given: 

    #{inspect(config, pretty: true)}
    """
  end
end
