# Getting Started

## Installation

Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.0.0"},
      {:kadabra, "~> 0.2.0"}
    ]
  end
  ```
  
After running `mix deps.get`, if running Elixir `v1.3` or earlier, configure `mix.exs`
to start the applications automatically.
  ```elixir
  def application do
    [applications: [:pigeon, :kadabra]]
  end
  ```

*For details about sending push notifications, see the guides for APNS, FCM, and ADM.*
