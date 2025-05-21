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
               | requests: %{ref => %Request{error: :timeout}}
             }
    end

    test "ignores unexpected responses", %{queue: queue} do
      result = RequestQueue.process([{:not_real, :erlang.make_ref()}], queue)
      assert result == queue
    end
  end
end
