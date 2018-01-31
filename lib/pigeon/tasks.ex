defmodule Pigeon.Tasks do
  @moduledoc false

  def process_on_response(nil, _notif), do: :ok

  def process_on_response(on_response, notif) do
    Task.Supervisor.start_child(Pigeon.Tasks, fn -> on_response.(notif) end)
  end
end
