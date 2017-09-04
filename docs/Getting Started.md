# Getting Started

## Installation

Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.1.0"},
      {:kadabra, "~> 0.3.1"}
    ]
  end
  ```

*For details about sending push notifications, see the guides for APNS, FCM, and ADM.*

Want to swap Kadabra with another HTTP2 client? See `Pigeon.Http2.Client`
