defmodule Pigeon.Mixfile do
  use Mix.Project

  def project do
    [app: :pigeon,
     name: "Pigeon",
     version: "0.3.0",
     elixir: "~> 1.2",
     source_url: "https://github.com/codedge-llc/pigeon",
     description: description,
     package: package,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :httpoison],
    mod: {Pigeon, []}]
  end

  defp deps do
    [{:poison, "~> 1.5"},
    {:httpoison, "~> 0.7"},
    {:earmark, "~> 0.1", only: :dev},
    {:ex_doc, "~> 0.11", only: :dev}]
  end

  defp description do
    """
    A wrapper for sending iOS and Android push notifications.
    """
  end

  defp package do
    [
       files: ["lib", "mix.exs", "README*", "LICENSE*"],
       maintainers: ["Henry Popp", "Tyler Hurst"],
       licenses: ["MIT"],
       links: %{"GitHub" => "https://github.com/codedge-llc/pigeon"}
    ]
  end
end
