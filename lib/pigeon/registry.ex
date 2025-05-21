defmodule Pigeon.Registry do
  @moduledoc false

  def child_spec(_opts \\ []) do
    %{
      id: __MODULE__,
      start: {Registry, :start_link, [[keys: :duplicate, name: __MODULE__]]},
      type: :supervisor
    }
  end

  @spec register(pid()) :: {:ok, pid()} | {:error, term()}
  def register(pid) do
    Registry.register(__MODULE__, pid, nil)
  end

  @spec unregister(pid()) :: :ok | {:error, term()}
  def unregister(pid) do
    Registry.unregister(__MODULE__, pid)
  end

  @spec next(pid()) :: pid() | nil
  def next(pid) do
    __MODULE__
    |> Registry.lookup(pid)
    |> case do
      [] -> nil
      pids -> pids |> Enum.random() |> elem(0)
    end
  end
end
