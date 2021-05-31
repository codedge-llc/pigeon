defmodule Pigeon.Mixfile do
  use Mix.Project

  @source_url "https://github.com/codedge-llc/pigeon"
  @version "2.0.0-dev"

  def project do
    [
      app: :pigeon,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      dialyzer: [
        plt_add_apps: [:kadabra],
        ignore_warnings: "config/dialyzer.ignore-warnings"
      ],
      docs: docs(),
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
      source_url: @source_url,
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      version: @version
    ]
  end

  defp elixirc_paths(:dev), do: ["lib", "test/support"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      extra_applications: [:logger],
      mod: {Pigeon.Application, []}
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:earmark, "~> 1.0", only: :dev, runtime: false},
      {:excoveralls, "~> 0.5", only: :test, runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:gen_stage, "~> 0.12 or ~> 1.0"},
      {:goth, "~> 1.3.0-rc.2"},
      {:httpoison, "~> 0.7 or ~> 1.0"},
      {:jason, "~> 1.0", optional: true},
      {:joken, "~> 2.1"},
      {:kadabra, "~> 0.6.0"}
    ]
  end

  defp docs do
    [
      groups_for_modules: [
        "ADM - Amazon Android": [Pigeon.ADM, Pigeon.ADM.Notification],
        "APNS - Apple iOS": [Pigeon.APNS, Pigeon.APNS.Notification],
        "FCM - Firebase Cloud Messaging": [
          Pigeon.FCM,
          Pigeon.FCM.Notification,
          Pigeon.LegacyFCM,
          Pigeon.LegacyFCM.Notification
        ],
        "Advanced Configuration": [Pigeon.Adapter, Pigeon.Dispatcher]
      ],
      main: "Pigeon"
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
      links: %{
        "Changelog" => "https://hexdocs.pm/pigeon/changelog.html",
        "GitHub" => @source_url
      },
      maintainers: ["Henry Popp", "Tyler Hurst"]
    ]
  end
end
