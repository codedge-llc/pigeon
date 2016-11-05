defmodule Pigeon.ADM do
  @moduledoc """
    Defines publically-exposed Amazon Device Messaging (ADM) functions. For implementation
    see ADMWorker.
  """

  @doc """
    Sends a push over ADM.
  """
  @spec push(Pigeon.ADM.Notification) :: none
  def push(notification), do: GenServer.cast(:adm_worker, {:push, :adm, notification})

  @doc """
    Sends a push over ADM.
  """
  @spec push(Pigeon.ADM.Notification, (() -> none)) :: none
  def push(notification, on_response),
    do: GenServer.cast(:adm_worker, {:push, :adm, notification, on_response})
end
