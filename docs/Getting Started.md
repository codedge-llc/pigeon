# Getting Started

## Installation
**Note: Pigeon's API will likely change until v1.0**

Add pigeon as a `mix.exs` dependency:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 0.11.0"}
    ]
  end
  ```
  
After running `mix deps.get`, if running Elixir `v1.3` or earlier, configure `mix.exs`
to start the application automatically.
  ```elixir
  def application do
    [applications: [:pigeon]]
  end
  ```

*For details about sending push notifications, see the guides for APNS, GCM, and ADM.*
