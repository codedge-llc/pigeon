[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Deps Status](https://beta.hexfaktor.org/badge/all/github/codedge-llc/pigeon.svg)](https://beta.hexfaktor.org/github/codedge-llc/pigeon)
# Pigeon
HTTP2-compliant wrapper for sending iOS and Android push notifications.

## Installation
**Note: Pigeon's API will likely change until v1.0**

Add pigeon as a `mix.exs` dependency:
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
  
## Getting Started
For usage and configuration, see the docs:
* [APNS (Apple iOS)](https://hexdocs.pm/pigeon/apns-apple-ios.html)
* [GCM (Android)](https://hexdocs.pm/pigeon/gcm-android.html)
* [ADM (Amazon Android)](https://hexdocs.pm/pigeon/adm-amazon-android.html)
