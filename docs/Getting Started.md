# Getting Started

## Installation
**Note: Pigeon's API will likely change until v1.0**

Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 0.12.0"},
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
  

Prefer `chatterbox` as your http2 client?
  ```elixir
  def deps do
    [
      {:pigeon, "~> 0.12.0"},
      {:chatterbox, "~> 0.4.0"}
    ]
  end
  ```

Specify it in your `config.exs`
  ```elixir
  :config, :pigeon, http2_client: Pigeon.Http2.Client.Chatterbox
  ```

*For details about sending push notifications, see the guides for APNS, GCM, and ADM.*
