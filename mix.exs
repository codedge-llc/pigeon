defmodule Pigeon.Mixfile do
  use Mix.Project

  @description "iOS (APNS), Android (FCM), and Amazon Android (ADM) push notifications for Elixir."
  @name "Pigeon"
  @source_url "https://github.com/codedge-llc/pigeon"
  @version "2.1.0"

  def project do
    [
      app: :pigeon,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      description: @description,
      dialyzer: dialyzer(),
      docs: docs(),
      elixir: "~> 1.7",
      elixirc_options: [warnings_as_errors: true],
      elixirc_paths: elixirc_paths(Mix.env()),
      name: @name,
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
      {:goth, "~> 1.4.3"},
      {:jason, "~> 1.0", optional: true},
      {:joken, "~> 2.1"},
      {:mint, "~> 1.0"}
    ]
  end

  defp docs do
    [
      extras: [
        "CHANGELOG.md",
        LICENSE: [title: "License"],
        "docs/Migrating-to-v2-0-0.md": [title: "Upgrading to v2.0"]
      ],
      groups_for_modules: [
        "ADM - Amazon Android": [Pigeon.ADM, Pigeon.ADM.Notification],
        "APNS - Apple iOS": [Pigeon.APNS, Pigeon.APNS.Notification],
        "FCM - Firebase Cloud Messaging": [
          Pigeon.FCM,
          Pigeon.FCM.Notification
        ]
      ],
      formatters: ["html"],
      main: "Pigeon",
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"],
      source_ref: "v#{@version}",
      source_url: @source_url
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG*"],
      licenses: ["MIT"],
      links: %{
        "Changelog" => "https://hexdocs.pm/pigeon/changelog.html",
        "GitHub" => @source_url,
        "Sponsor" => "https://github.com/sponsors/codedge-llc"
      },
      maintainers: ["Henry Popp", "Tyler Hurst"]
    ]
  end

  defp dialyzer do
    [
      plt_file: {:no_warn, "priv/plts/dialyzer.plt"}
    ]
  end
end
