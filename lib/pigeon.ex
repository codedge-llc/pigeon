defmodule Pigeon do
  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  @doc """
  Returns the configured JSON encoding library for Pigeon.
  To customize the JSON library, include the following in your config/config.exs:

      config :pigeon, :json_library, Jason
  """
  @spec json_library :: module
  def json_library do
    Application.get_env(:pigeon, :json_library, Jason)
  end

  @doc false
  def start_connection(state) do
    Supervisor.start_child(:pigeon, {Pigeon.Connection, state})
  end

  def debug_log?, do: Application.get_env(:pigeon, :debug_log, false)
end
