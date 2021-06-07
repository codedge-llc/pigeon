defmodule Pigeon.Adapter do
  @moduledoc """
  Adapter behaviour for `Pigeon.Dispatcher` push workers.

  `Pigeon.Adapter` closely resembles `GenServer` behaviour, as dispatchers
  are GenServers under the hood.

  ## Example Adapter

  ```
  defmodule Pigeon.Sandbox do
    import Pigeon.Tasks, only: [process_on_response: 1]

    @behaviour Pigeon.Adapter

    @impl true
    def init(opts \\ []) do
      {:ok, opts}
    end

    @impl true
    def handle_info(_msg, state) do
      {:noreply, state}
    end

    @impl true
    def handle_push(%{response: nil} = notification, on_response, _state) do
      process_on_response(on_response, %{notification | response: :success})
      {:noreply, state}
    end

    def handle_push(notification, on_response, state) do
      process_on_response(on_response, notification)
      {:noreply, state}
    end
  end
  ```
  """

  @doc """
  Invoked when the server is started.

  Return value should be `{:ok, state}` for the `Pigeon.Dispatcher` state,
  or `{:stop, atom}` if started with invalid configuration options.
  """
  @callback init(opts :: Keyword.t()) :: {:ok, any} | {:stop, any}

  @doc """
  Invoked to handle all other messages.
  """
  @callback handle_info(term, term) :: {:noreply, term}

  @doc """
  Invoked to handle push notifications.
  """
  @callback handle_push(notification :: struct | [struct], state :: term) ::
              {:noreply, new_state :: term}
              | {:stop, reason :: term, new_state :: term}
end
