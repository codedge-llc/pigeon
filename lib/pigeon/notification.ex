defmodule Pigeon.Notification do
  @moduledoc """
  Contains shared functions for GCM and APNS.
  """
  require Logger

  def json_payload(payload) do
    response = Poison.encode(payload)
    case response do
    {:ok, result} ->
      result
    {:error, error} ->
      Logger.error error
    end
  end
end
