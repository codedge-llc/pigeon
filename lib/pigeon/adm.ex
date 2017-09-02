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
    GenServer.cast(worker_name, {:push, :adm, notification})
  end

  @doc """
  Sends a push over ADM.
  """
  @spec push(Notification.t, (() -> none), Keyword.t) :: no_return
  def push(notification, on_response, opts) do
    worker_name = opts[:to] || Config.default_name
    GenServer.cast(worker_name, {:push, :adm, notification, on_response})
  end
end
