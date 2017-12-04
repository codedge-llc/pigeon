defmodule Pigeon.Mixfile do
  use Mix.Project

  @version "1.1.2"

  def project do
    [
      app: :pigeon,
      name: "Pigeon",
      version: @version,
      elixir: "~> 1.4",
   	  elixirc_paths: elixirc_paths(Mix.env),
      source_url: "https://github.com/codedge-llc/pigeon",
      description: description(),
      package: package(),
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        "coveralls": :test,
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
          "docs/Migrating to v1-1-0.md",
          "CHANGELOG.md"
        ]
      ],
      deps: deps()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_),     do: ["lib"]

  def application do
    [extra_applications: [:logger],
    mod: {Pigeon, []}]
  end

  defp deps do
    [
      {:poison, "~> 2.0 or ~> 3.0"},
      {:httpoison, "~> 0.7"},
      {:gen_stage, "~> 0.12.0"},
      {:kadabra, "~> 0.3.5", optional: true},
      {:earmark, "~> 1.0", only: :dev},
      {:ex_doc, "~> 0.2", only: :dev},
      {:excoveralls, "~> 0.5", only: :test},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:credo, "~> 0.8", only: [:dev, :test], runtime: false}
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
