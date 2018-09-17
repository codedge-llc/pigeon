![logo](https://raw.githubusercontent.com/codedge-llc/pigeon/master/docs/logo.png)
> HTTP2-compliant wrapper for sending iOS and Android push notifications.

[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Coverage Status](https://coveralls.io/repos/github/codedge-llc/pigeon/badge.svg)](https://coveralls.io/github/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)

## Installation

Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.2.3"},
      {:kadabra, "~> 0.4.3"}
    ]
  end
  ```

*For details about sending push notifications, see the guides for APNS, FCM, and ADM.*

Want to swap Kadabra with another HTTP2 client? See `Pigeon.Http2.Client`
