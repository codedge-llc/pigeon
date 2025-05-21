defmodule Pigeon.Tasks do
  @moduledoc false

  @spec process_on_response(Pigeon.notification()) :: :ok
  def process_on_response(%{__meta__: %{on_response: nil}}), do: :ok

  def process_on_response(%{__meta__: %{on_response: {m, f}}} = notif) do
    Task.Supervisor.start_child(Pigeon.Tasks, fn ->
      :erlang.apply(m, f, [notif])
    end)
  end

  def process_on_response(%{__meta__: %{on_response: {m, f, a}}} = notif) do
    Task.Supervisor.start_child(Pigeon.Tasks, fn ->
      :erlang.apply(m, f, [notif] ++ a)
    end)
  end

  def process_on_response(%{__meta__: %{on_response: fun}} = notif)
      when is_function(fun, 1) do
    Task.Supervisor.start_child(Pigeon.Tasks, fn -> fun.(notif) end)
  end
end
