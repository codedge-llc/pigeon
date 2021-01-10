defmodule Pigeon.Adapter do
  @type on_response :: (Notification.t() -> no_return)

  @callback handle_info(term, term) :: {:noreply, term}

  @callback handle_push(notification :: struct | [struct], on_response, state :: term) ::
              term

  @callback initial_state(opts :: Keyword.t()) :: term
end
