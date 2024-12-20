# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.13.1 - 2018-01-09

- Fixed typespec compile errors for Elixir v1.6

## v0.13.0 - 2017-05-17

- Configurable `:ping_period` for APNS connections

## v0.12.1 - 2017-04-02

- Various `chatterbox` client adapter fixes

## V0.12.0 - 2017-04-01

- Configurable `Pigeon.Http2.Client`. Currently supports `kadabra`
  and `chatterbox`
- `kadabra` bumped to `v0.2.0`

## v0.11.0 - 2017-03-07

- APNS workers can be started and referenced with pids and/or atom names
- Fix: Push `:name` option renamed to `:to`
- Fix: GCM/ADM async pushes now use `spawn/1` instead of `Task.async/1`

## v0.10.3 - 2017-01-06

- Fix: cleaned up Elixir v1.4 warnings

## v0.10.2 - 2016-11-27

- Fix: poison dependency version made optionally `~> 2.0 or ~> 3.0`

## v0.10.1 - 2016-11-21

- Fix: kadabra not started

## v0.10.0 - 2017-11-13

- Migrated HTTP/2 client from `chatterbox` to `kadabra`
- Support for ADM (Amazon Android) push
- APNS pushes are now synchronous by default. For async pushes use the
  new `on_response` option. GCM and ADM will have it in the next major release.
- Bulk APNS pushing
- Handling of multiple APNS worker connections with different configs
- Re-implemented APNS socket pings to keep connections open
- `:invalid_jSON` corrected to `:invalid_json`

## v0.9.2

- Fixed GCM error response atom conversion

## v0.9.1 - 2016-09-01

- Fixed :eaddrinuse error when restarting Pigeon too quickly with
  :apns_2197 enabled

## v0.9.0 - 2016-07-20

- APNS topic made optional
- APNS `put_mutable_content` helper function added
- GCM can be configured on a per-push basis
- Updated to use Macro.underscore

## v0.8.0 - 2016-06-23

- Implemented Chatterbox as APNS HTTP2 client
- APNS server responses now caught asynchronously
- GCM support for `notification` and `data` payload keys
  (`Pigeon.GCM.Notification.new` API changes)

## v0.7.0 - 2016-06-02

- APNS cert/key configs can now either be a file path, full-text string,
  or `{:your_app, "path/to/file.pem"}` (which looks in the `/priv` directory
  of your app)
- Fixed APNSWorker crash on `:ssl.send/2` timeout
- Better error-handling for invalid APNS configs

## v0.6.0 - 2016-05-20

- `Pigeon.APNS.Notification.new/3` returns `%Pigeon.APNS.Notification{}` struct
- Configure APNS to use SSL port 2197 with `apns_2197: true` in
  your `config.exs`
- Error feedback symbols converted from `CamelCase` to `snake_case`
- APNS expiration values supported with `expiration` key in
  `%Pigeon.APNS.Notification{}`

## v0.5.2 - 2016-05-03

- Fixed bug where APNSWorker would hang up if SSL connection failed. Now
  retries the connection twice more before gracefully shutting down.

## v0.5.1 - 2016-03-30

- GCM error responses return proper chunk of regstration IDs

## v0.5.0 - 2016-03-30

- `Pigeon.GCM.Notification.new/2` returns `%Pigeon.GCM.Notification{}` struct
- Multiple registration IDs allowed in `Pigeon.GCM.push/2`
- Remove `:gcm_worker`
