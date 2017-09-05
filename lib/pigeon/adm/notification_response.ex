defmodule Pigeon.ADM.NotificationResponse do
  @moduledoc """
  Passed to the ADM on_response callback
  """
  defstruct ok: [],
            retry: [],
            update: [],
            remove: [],
            error: %{}

  @type t :: %__MODULE__{
    ok: [String.t, ...],
    retry: [String.t],
    update: [{String.t, String.t}, ...],
    remove: [String.t, ...],
    error: %{String.t => [String.t, ...]}
  }

  # Combines multiple NotificationResponses into one
  def new(responses) do
    Enum.reduce(responses, %__MODULE__{}, fn(response, acc) ->
      case response do
        {:ok, r} -> put_response(acc, r)
        _ -> acc
      end
    end)
  end

  defp put_response(acc, {:ok, response}) do
    %{ acc |
       ok: acc.ok ++ response.ok,
       retry: acc.retry ++ response.retry,
       update: acc.update ++ response.update,
       remove: acc.remove ++ response.remove
    }
  end

  defp put_response(%{error: error} = resp, {:error, reason, notification}) do
    similar = error[reason] || []
    errors = Map.put(error, reason, [notification | similar])
    %{resp | error: errors}
  end

end
