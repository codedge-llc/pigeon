defmodule Pigeon.RegistryTest do
  use ExUnit.Case, async: false

  test "register/1 registers a pid" do
    Pigeon.Registry.register(Pigeon.Example)
    assert Pigeon.Registry.next(Pigeon.Example) == self()

    Pigeon.Registry.unregister(Pigeon.Example)
  end

  test "unregister/1 removes a pid" do
    Pigeon.Registry.register(Pigeon.Example)
    Pigeon.Registry.unregister(Pigeon.Example)
    refute Pigeon.Registry.next(Pigeon.Example) == self()
  end
end
