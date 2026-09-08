defmodule Pigeon.HTTP.RequestQueueTest do
  use ExUnit.Case, async: true
  doctest Pigeon.HTTP.RequestQueue

  alias Pigeon.HTTP.{Request, RequestQueue}

  setup do
    %{queue: RequestQueue.new()}
  end

  describe "process/2" do
    test "handles error responses", %{queue: queue} do
      ref = :erlang.make_ref()
      queue = RequestQueue.add(queue, ref, nil)

      responses = [
        {:error, ref, :timeout}
      ]

      assert RequestQueue.process(responses, queue) == %{
               queue
               | requests: %{ref => %Request{error: :timeout, done?: true}}
             }
    end

    test "errored requests pop as done", %{queue: queue} do
      ref = :erlang.make_ref()

      {done, queue} =
        queue
        |> RequestQueue.add(ref, :notif)
        |> then(&RequestQueue.process([{:error, ref, :reset}], &1))
        |> RequestQueue.pop_done()

      [{^ref, %Request{error: :reset, notification: :notif}}] = done
      assert RequestQueue.empty?(queue)
    end

    test "ignores unexpected responses", %{queue: queue} do
      result = RequestQueue.process([{:not_real, :erlang.make_ref()}], queue)
      assert result == queue
    end
  end

  describe "drain/1" do
    test "returns every request and empties the queue", %{queue: queue} do
      refute RequestQueue.empty?(RequestQueue.add(queue, make_ref(), :a))

      queue =
        queue
        |> RequestQueue.add(make_ref(), :a)
        |> RequestQueue.add(make_ref(), :b)

      {requests, queue} = RequestQueue.drain(queue)

      assert Enum.sort(Enum.map(requests, & &1.notification)) == [:a, :b]
      assert RequestQueue.empty?(queue)
    end
  end
end
