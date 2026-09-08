defmodule Pigeon.HTTP do
  @moduledoc false

  alias Pigeon.HTTP.RequestQueue
  require Logger

  @type handler :: (Pigeon.HTTP.Request.t() -> any())

  @spec handle_info(term(), term(), handler()) :: {:noreply, term()}
  def handle_info(message, state, handler) do
    %{queue: queue, socket: socket} = state

    case Mint.HTTP.stream(socket, message) do
      :unknown ->
        {:noreply, state}

      {:ok, socket, responses} ->
        {done, queue} =
          responses
          |> RequestQueue.process(queue)
          |> RequestQueue.pop_done()

        handle_request_notifications(done, handler)

        {:noreply, %{state | queue: queue, socket: socket}}

      {:error, socket, error, _responses} ->
        error |> inspect(pretty: true) |> Logger.error()
        {:noreply, %{state | socket: socket}}
    end
  end

  defp handle_request_notifications(finished_requests, handler) do
    for {_ref, request} <- finished_requests do
      if request.notification, do: handler.(request)
    end
  end
end
