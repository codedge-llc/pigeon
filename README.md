[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Deps Status](https://beta.hexfaktor.org/badge/all/github/codedge-llc/pigeon.svg)](https://beta.hexfaktor.org/github/codedge-llc/pigeon)
# Pigeon
HTTP2-compliant wrapper for sending iOS and Android push notifications.

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
  

Prefer `chatterbox` as your http2 client?
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.0.0"},
      {:chatterbox, "~> 0.4.0"}
    ]
  end
  ```

Specify it in your `config.exs`
  ```elixir
  config, :pigeon, http2_client: Pigeon.Http2.Client.Chatterbox
  ```

## Getting Started
For usage and configuration, see the docs:
* [APNS (Apple iOS)](https://hexdocs.pm/pigeon/apns-apple-ios.html)
* [GCM (Android)](https://hexdocs.pm/pigeon/gcm-android.html)
* [ADM (Amazon Android)](https://hexdocs.pm/pigeon/adm-amazon-android.html)
