defmodule Pigeon.Mixfile do
  use Mix.Project

  def project do
    [app: :pigeon,
     version: "0.1.0",
     elixir: "~> 1.0",
     description: description,
     package: package]
  end

  defp description do
    """
    Pigeon is a wrapper for sending iOS push notifications.
    """
  end

  defp package do
    [
       files: ["lib", "mix.exs", "README*", "LICENSE*"],
       contributors: ["Henry Popp"],
       licenses: ["MIT"],
       links: %{"GitHub" => "https://github.com/codedge-llc/pigeon"}
    ]
  end
end
