defmodule Pigeon.Mixfile do
  use Mix.Project

  def project do
    [app: :pigeon,
     name: "Pigeon",
     version: "0.10.3",
     elixir: "~> 1.2",
     source_url: "https://github.com/codedge-llc/pigeon",
     description: description(),
     package: package(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: ["coveralls": :test, "coveralls.detail": :test,
      "coveralls.post": :test, "coveralls.html": :test],
     docs: [main: "getting-started",
            extras: ["docs/Getting Started.md",
                     "docs/APNS Apple iOS.md",
                     "docs/GCM Android.md",
                     "docs/ADM Amazon Android.md",
                     "CHANGELOG.md"]],
     deps: deps()]
  end

  def application do
    [applications: [:logger, :httpoison, :poolboy, :chatterbox, :hpack],
    mod: {Pigeon, []}]
  end

  defp deps do
    [{:poison, "~> 2.0 or ~> 3.0"},
    {:httpoison, "~> 0.7"},
    {:chatterbox, github: "rslota/chatterbox"},
    {:poolboy, "~> 1.5"},
    {:dogma, "~> 0.1", only: :dev},
    {:earmark, "~> 1.0", only: :dev},
    {:ex_doc, "~> 0.2", only: :dev},
    {:excoveralls, "~> 0.5", only: :test}]
  end

  defp description do
    """
    HTTP2-compliant wrapper for sending iOS (APNS), Android (GCM), and Amazon Android (ADM) push notifications.
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
