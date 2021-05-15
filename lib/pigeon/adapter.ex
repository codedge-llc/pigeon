defmodule Pigeon.Adapter do
  @type on_response :: (Notification.t() -> no_return)

  @callback init(opts :: Keyword.t()) :: {:ok, term} | {:error, atom}

  @callback handle_info(term, term) :: {:noreply, term}

  @callback handle_push(notification :: struct | [struct], on_response, state :: term) ::
              term
end
