defmodule Pigeon.FCM.NotificationResponse do
  @moduledoc """
  Passed to the FCM on_response callback
  """
  defstruct message_id: nil, ok: [], retry: [], update: [], remove: [], error: %{}

  @type t :: %__MODULE__{
    message_id: String.t,
    ok: [String.t, ...],
    retry: [String.t],
    update: [{String.t, String.t}, ...],
    remove: [String.t, ...],
    error: %{String.t => [String.t, ...]}
  }
end
