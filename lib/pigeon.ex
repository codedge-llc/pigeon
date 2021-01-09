defmodule Pigeon do
  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  import Supervisor.Spec

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
    opts = [restart: :temporary, id: :erlang.make_ref()]
    spec = worker(Pigeon.Connection, [state], opts)
    Supervisor.start_child(:pigeon, spec)
  end

  def debug_log?, do: Application.get_env(:pigeon, :debug_log, false)
end
