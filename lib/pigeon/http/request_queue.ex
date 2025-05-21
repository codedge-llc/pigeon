defmodule Pigeon.HTTP.RequestQueue do
  @moduledoc false

  defstruct requests: %{}

  @type t :: %__MODULE__{
          requests: %{reference() => Pigeon.HTTP.Request.t()}
        }

  alias Pigeon.HTTP.Request

  @doc ~S"""
  Creates a new request queue.

  ## Examples

      iex> Pigeon.HTTP.RequestQueue.new()
      %Pigeon.HTTP.RequestQueue{requests: %{}}
  """
  def new, do: %__MODULE__{}

  @doc ~S"""
  Adds a new request to the queue.

  ## Examples

      iex> queue = Pigeon.HTTP.RequestQueue.new()
      iex> ref = :erlang.make_ref()
      iex> Pigeon.HTTP.RequestQueue.add(queue, ref, nil)
      %Pigeon.HTTP.RequestQueue{requests: %{ref => %Pigeon.HTTP.Request{}}}
  """
  @spec add(t(), reference(), term()) :: t()
  def add(queue, request_ref, notification) do
    request = Request.new(notification)
    requests = Map.put(queue.requests, request_ref, request)
    %{queue | requests: requests}
  end

  @spec pop(t(), reference()) :: {Pigeon.HTTP.Request.t(), t()}
  def pop(queue, ref) do
    {request, requests} = Map.pop(queue.requests, ref)
    {request, %{queue | requests: requests}}
  end

  @spec pop_done(t()) :: {[Pigeon.HTTP.Request.t()], t()}
  def pop_done(queue) do
    {done, not_done} =
      Enum.split_with(queue.requests, fn {_ref, request} -> request.done? end)

    {done, %{queue | requests: Enum.into(not_done, %{})}}
  end

  @spec process([Mint.Types.response()] | Mint.Types.response(), t()) :: t()
  def process([], queue), do: queue

  def process([response | rest], queue) do
    process(rest, process(response, queue))
  end

  def process({:status, request_ref, status_code}, queue) do
    merge_result(queue, request_ref, %{status: status_code})
  end

  def process({:headers, request_ref, headers}, queue) do
    merge_result(queue, request_ref, %{headers: headers})
  end

  def process({:data, request_ref, body}, queue) do
    merge_result(queue, request_ref, %{body: body})
  end

  def process({:done, request_ref}, queue) do
    merge_result(queue, request_ref, %{done?: true})
  end

  def process({:error, request_ref, reason}, queue) do
    merge_result(queue, request_ref, %{error: reason})
  end

  def process(_other, queue), do: queue

  @spec merge_result(t(), reference(), map()) :: t()
  defp merge_result(%{requests: requests} = queue, ref, params) do
    request = requests[ref] || %Request{}

    new_requests =
      Map.put(requests, ref, Request.merge(request, params))

    %{queue | requests: new_requests}
  end
end
