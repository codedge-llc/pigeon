defmodule Pigeon.NotificationQueue do
  @moduledoc false

  @type queue :: %{required(pos_integer) => term}

  @doc ~S"""
  Returns a new empty queue.

  ## Examples

      iex> new()
      %{}
  """
  @spec new :: queue
  def new, do: %{}

  @doc ~S"""
  Adds a notification and on_response callback for given `stream_id`.

  ## Examples

      iex> add(%{}, 1, %Pigeon.APNS.Notification{})
      %{1 => %Pigeon.APNS.Notification{}}
  """
  @spec add(queue, pos_integer, term) :: queue
  def add(queue, stream_id, notification) do
    Map.put(queue, stream_id, notification)
  end

  @doc ~S"""
  Pops a notification and on_response callback for given `stream_id`.

  ## Examples

      iex> queue = %{1 => %Pigeon.APNS.Notification{}}
      iex> pop(queue, 1)
      {%Pigeon.APNS.Notification{}, %{}}

      iex> queue = %{1 => %Pigeon.APNS.Notification{}}
      iex> pop(queue, 3)
      {nil, %{1 => %Pigeon.APNS.Notification{}}}
  """
  @spec pop(queue, pos_integer) :: {nil | {term, term}, queue}
  def pop(queue, stream_id) do
    result = queue[stream_id]
    new_queue = Map.delete(queue, stream_id)
    {result, new_queue}
  end
end
