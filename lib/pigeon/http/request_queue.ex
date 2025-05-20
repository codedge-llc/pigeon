defmodule Pigeon.HTTP.RequestQueue do
  @moduledoc false

  defstruct requests: %{}

  alias Pigeon.HTTP.Request

  def new, do: %__MODULE__{}

  def add(queue, request_ref, notification) do
    request = Request.new(notification)
    requests = Map.put(queue.requests, request_ref, request)
    %{queue | requests: requests}
  end

  def pop_done(queue) do
    {done, not_done} =
      Enum.split_with(queue.requests, fn {_ref, request} -> request.done? end)

    {done, %{queue | requests: Enum.into(not_done, %{})}}
  end

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

  # def process({:pong, request_ref}, queue) do
  #   queue
  # end

  # def process({:push_promise, request_ref, promised_request_ref, headers}, queue) do
  #   queue
  # end

  defp merge_result(%{requests: requests} = queue, ref, params) do
    request = requests[ref] || %Request{}
    new_requests = Map.put(requests, ref, Request.merge(request, params))

    %{queue | requests: new_requests}
  end
end
