defmodule Pigeon.APNS.Token do
  @moduledoc false

  @type t :: {non_neg_integer(), binary() | nil}

  @spec start_link((() -> any())) :: Agent.on_start()
  def start_link(_) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec get(String.t()) :: t
  def get(name) do
    Agent.get(__MODULE__, &Map.get(&1, name, {0, nil}))
  end

  @spec update(String.t(), t) :: :ok
  def update(name, token) do
    Agent.update(__MODULE__, &Map.put(&1, name, token))
  end
end
