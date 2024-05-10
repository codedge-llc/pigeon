defmodule Pigeon.Registry do
  @moduledoc false

  def child_spec(_opts \\ []) do
    %{
      id: __MODULE__,
      start: {Registry, :start_link, [[keys: :duplicate, name: __MODULE__]]},
      type: :supervisor
    }
  end

  def register(pid, priority \\ 0) do
    Registry.register(__MODULE__, pid, priority)
  end

  def unregister(pid) do
    Registry.unregister(__MODULE__, pid)
  end

  def next(pid) do
    __MODULE__
    |> Registry.lookup(pid)
    |> Enum.min_by(fn {_, priority} -> priority end, fn -> {nil, 0} end)
    |> elem(0)
  end
end
