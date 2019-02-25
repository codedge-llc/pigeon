defmodule Pigeon.Mixfile do
  use Mix.Project

  @version "1.3.0"

  def project do
    [
      app: :pigeon,
      name: "Pigeon",
      version: @version,
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: "https://github.com/codedge-llc/pigeon",
      description: description(),
      package: package(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      dialyzer: [
        plt_add_apps: [:kadabra, :poison],
        ignore_warnings: "config/dialyzer.ignore-warnings"
      ],
      docs: [
        main: "getting-started",
        extras: [
          "docs/Getting Started.md",
          "docs/APNS Apple iOS.md",
          "docs/FCM Android.md",
          "docs/ADM Amazon Android.md",
          "CHANGELOG.md"
        ]
      ],
      deps: deps()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [extra_applications: [:logger], mod: {Pigeon, []}]
  end

  defp deps do
    [
      {:poison, "~> 2.0 or ~> 3.0"},
      {:httpoison, "~> 0.7 or ~> 1.0"},
      {:gen_stage, "~> 0.12"},
      {:joken, "~> 2.0.0"},
      {:kadabra, "~> 0.4.3", optional: true},
      {:earmark, "~> 1.0", only: :dev},
      {:ex_doc, "~> 0.18", only: :dev},
      {:excoveralls, "~> 0.5", only: :test},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false}
    ]
  end

  defp description do
    """
    HTTP2-compliant wrapper for sending iOS (APNS), Android (FCM),
    and Amazon Android (ADM) push notifications.
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
