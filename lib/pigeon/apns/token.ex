defmodule Pigeon.APNS.Token do
  @moduledoc false

  use Agent, restart: :permanent, shutdown: 5_000

  @spec start_link((() -> any())) :: Agent.on_start()
  def start_link(_) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec get(atom()) :: {non_neg_integer(), binary()}
  def get(name) do
    Agent.get(__MODULE__, &Map.get(&1, name, {0, nil}))
  end

  @spec update(atom(), non_neg_integer(), binary()) :: :ok
  def update(name, timestamp, token) do
    Agent.update(__MODULE__, &Map.put(&1, name, {timestamp, token}))
  end
end
