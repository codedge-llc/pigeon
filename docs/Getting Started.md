# Getting Started

## Installation
**Note: Pigeon's API will likely change until v1.0**

Add pigeon and chatterbox as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 0.10.0"}
    ]
  end
  ```
  
After running `mix deps.get`, configure `mix.exs` to start the application automatically.
  ```elixir
  def application do
    [applications: [:pigeon]]
  end
  ```

*For details about sending push notifications, see the guides for APNS, GCM, and ADM.*
