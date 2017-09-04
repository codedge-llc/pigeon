defmodule Pigeon.APNS.NotificationResponse do
  defstruct ok: [], error: %{}

  alias Pigeon.APNS.NotificationResponse

  def new(responses) do
    Enum.reduce(responses, %NotificationResponse{}, fn(response, acc) ->
      case response do
        {:ok, r} -> put_response(acc, r)
        _ -> acc
      end
    end)
  end

  defp put_response(%{ok: ok} = resp, {:ok, notification}) do
    %{resp | ok: [notification | ok]}
  end

  defp put_response(%{error: error} = resp, {:error, reason, notification}) do
    similar = error[reason] || []
    errors = Map.put(error, reason, [notification | similar])
    %{resp | error: errors}
  end
end
