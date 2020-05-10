defmodule Pigeon.Mixfile do
  use Mix.Project

  @version "1.5.1"

  def project do
    [
      app: :pigeon,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      dialyzer: [
        plt_add_apps: [:kadabra, :poison],
        ignore_warnings: "config/dialyzer.ignore-warnings"
      ],
      docs: [
        main: "getting-started",
        extras: [
          {"README.md", [filename: "getting-started", title: "Getting Started"]},
          "docs/APNS Apple iOS.md",
          "docs/FCM Android.md",
          "docs/ADM Amazon Android.md",
          "CHANGELOG.md"
        ]
      ],
      elixir: "~> 1.6",
      elixirc_options: [warnings_as_errors: true],
      elixirc_paths: elixirc_paths(Mix.env()),
      name: "Pigeon",
      package: package(),
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      source_url: "https://github.com/codedge-llc/pigeon",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      version: @version
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [extra_applications: [:logger], mod: {Pigeon, []}]
  end

  defp deps do
    [
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:earmark, "~> 1.0", only: :dev},
      {:excoveralls, "~> 0.5", only: :test},
      {:ex_doc, "~> 0.18", only: :dev},
      {:gen_stage, "~> 0.12 or ~> 1.0"},
      {:httpoison, "~> 0.7 or ~> 1.0"},
      {:joken, "~> 2.1"},
      {:kadabra, "~> 0.4.3", optional: true},
      {:poison, "~> 2.0 or ~> 3.0 or ~> 4.0"}
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
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/codedge-llc/pigeon"},
      maintainers: ["Henry Popp", "Tyler Hurst"]
    ]
  end
end
