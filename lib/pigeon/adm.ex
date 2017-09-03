defmodule Pigeon.ADM do
  @moduledoc """
  Defines publically-exposed Amazon Device Messaging (ADM) functions. For implementation
  see ADM.Worker.
  """

  alias Pigeon.ADM.{Config, Notification}

  @doc """
  Sends a push over ADM.
  """
  @spec push(Notification.t, Keyword.t) :: no_return
  def push(notification, opts \\ []) do
    worker_name = opts[:to] || Config.default_name
    case opts[:on_response] do
      nil -> GenServer.cast(worker_name, {:push, :adm, notification})
      on_response -> GenServer.cast(worker_name, {:push, :adm, notification, on_response})
    end
  end
end
