![logo](https://raw.githubusercontent.com/codedge-llc/pigeon/master/docs/logo.png)

> HTTP2-compliant wrapper for sending iOS and Android push notifications.

[![CI](https://github.com/codedge-llc/pigeon/actions/workflows/ci.yml/badge.svg)](https://github.com/codedge-llc/pigeon/actions/workflows/ci.yml)
[![Version](https://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Total Downloads](https://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
[![License](https://img.shields.io/hexpm/l/pigeon.svg)](https://github.com/codedge-llc/pigeon/blob/master/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/codedge-llc/pigeon.svg)](https://github.com/codedge-llc/pigeon/commits/master)
[![Documentation](https://img.shields.io/badge/documentation-gray)](https://hexdocs.pm/pigeon/)

_Pigeon v2.0 is in release candidate status. See [the latest stable 1.6 on Hex](https://hex.pm/packages/pigeon)
or [the 1.6 branch on GitHub](https://github.com/codedge-llc/pigeon/tree/v1.6) for installation._

## Installation

Add `:pigeon` and as a `mix.exs` dependency:

```elixir
def deps do
  [
    {:pigeon, "~> 2.0.0-rc.3"}
  ]
end
```

## Getting Started

Check the module documentation for your push notification service.

- [Pigeon.ADM](https://hexdocs.pm/pigeon/2.0.0-rc.2/Pigeon.ADM.html) - Amazon Android.
- [Pigeon.APNS](https://hexdocs.pm/pigeon/2.0.0-rc.2/Pigeon.APNS.html) - Apple iOS.
- [Pigeon.FCM](https://hexdocs.pm/pigeon/2.0.0-rc.2/Pigeon.FCM.html) - Firebase Cloud Messaging v1 API.

### Creating Dynamic Runtime Dispatchers

Pigeon can spin up dynamic dispatchers for a variety of advanced use-cases, such as
supporting dozens of dispatcher configurations or custom connection pools.

See [Pigeon.Dispatcher](https://hexdocs.pm/pigeon/2.0.0-rc.1/Pigeon.Dispatcher.html) for instructions.

### Writing a Custom Dispatcher Adapter

Want to write a Pigeon adapter for an unsupported push notification service?

See [Pigeon.Adapter](https://hexdocs.pm/pigeon/2.0.0-rc.1/Pigeon.Adapter.html) for instructions.

## Contributing

### Testing

Unit tests can be run with `mix test` or `mix coveralls.html`. Environment variables will need to be set for
various credentials. See [config/test.exs](https://github.com/codedge-llc/pigeon/blob/master/config/test.exs)
for the full list.

### Formatting

This project uses Elixir's `mix format` and [Prettier](https://prettier.io) for formatting.
Add hooks in your editor of choice to run it after a save. Be sure it respects this project's
`.formatter.exs`.

### Commits

Git commit subjects use the [Karma style](http://karma-runner.github.io/5.0/dev/git-commit-msg.html).

## License

Copyright (c) 2015-2024 Codedge LLC (https://www.codedge.io/)

This library is MIT licensed. See the [LICENSE](https://github.com/codedge-llc/pigeon/blob/master/LICENSE) for details.
