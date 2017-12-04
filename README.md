![logo](https://raw.githubusercontent.com/codedge-llc/pigeon/master/docs/logo.png)
> HTTP2-compliant wrapper for sending iOS and Android push notifications.

[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Coverage Status](https://coveralls.io/repos/github/codedge-llc/pigeon/badge.svg)](https://coveralls.io/github/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Deps Status](https://beta.hexfaktor.org/badge/all/github/codedge-llc/pigeon.svg)](https://beta.hexfaktor.org/github/codedge-llc/pigeon)

## Installation

Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.1.2"},
      {:kadabra, "~> 0.3.5"}
    ]
  end
  ```

## Getting Started
For usage and configuration, see the docs:
* [APNS (Apple iOS)](https://hexdocs.pm/pigeon/apns-apple-ios.html)
* [FCM (Android)](https://hexdocs.pm/pigeon/fcm-android.html)
* [ADM (Amazon Android)](https://hexdocs.pm/pigeon/adm-amazon-android.html)

Looking for GCM? Try `v0.13` or earlier.
