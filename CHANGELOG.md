# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.6.3 - 2024-08-12

**Fixed**

- Clean up Elixir 1.17 deprecation warnings ([#262](https://github.com/codedge-llc/pigeon/pull/262)).

## 1.6.2 - 2024-01-17

**Changed**

- Relax Poison dependency to allow 5.x.
- Support HTTPoison 2.0, dropping support for ~> 0.7.

**Fixed**

- Handle new PEM decode case for APNS cert keys with OpenSSL 3.0.

## v1.6.1 - 2021-05-31

**Security**

- Bump minimum Kadabra to 0.6.0. This is a critical security update for CA certificate validation.

## v1.6.0 - 2021-01-09

**Added**

- JSON library made configurable. For backwards compatibility, Poison is still a required dependency.
  Override in your `config.exs`.

  ```
  config :pigeon, json_library: Jason
  ```

**Fixed**

- Handle FCM single message_id's on topic pushes.

## v1.5.1 - 2020-05-25

- Added APNS InvalidPushType error ([#172](https://github.com/codedge-llc/pigeon/pull/172)).
- Fixed various typespecs ([#170](https://github.com/codedge-llc/pigeon/pull/170)).

## v1.5.0 - 2020-02-17

- Bumped minimum Elixir version to 1.6
- Raise `Pigeon.ConfigError` when booting invalid config structs.
  See below for validated keys and error types.
- `APNS.JWTConfig` now validates key p8 content before connecting.
- Relaxed `gen_stage` dependency to allow `~> 1.0`

Validated config keys:

- `ADM.Config` - `:client_id`, `:client_secret`
- `APNS.Config` - `:cert`, `:key`
- `APNS.JWTConfig` - `:team_id`, `:key`, `:key_identifier`
- `FCM.Config` - `:key`

Possible error values:

- `{:error, {:invalid, value}}`
- `{:error, {:nofile, value}}`

## v1.4.0 - 2019-10-13

- `apns-push-type` header support for iOS 13. An additional `:push_type` key has been
  added to the `APNS.Notification` struct.

## v1.3.2 - 2019-08-17

- Document workers configuration for run-time configuration of push workers.
- Modify run-time configuration of push workers so that multiple (or no)
  workers may be returned by the startup configuration.

## v1.3.1 - 2019-06-06

- Joken dependency bumped to 2.1

## v1.3.0 - 2019-03-14

- Support for FCM `content_available`, `mutable_content`, and `condition` keys
- Set `priority` of APNS notifications
- Joken dependency bumped to 2.0.1

## v1.2.4 - 2018-10-15

- Fixed ADM handling of connection timeouts

## v1.2.3 - 2018-09-17

- Fixed APNS, FCM and ADM error response parse crashes. Error responses not
  listed in the documentation are returned as `:unknown_error`

## v1.2.2 - 2018-07-08

- Fixed APNS handling of notification `expiration`
- Added APNS support for `collapse_id`

## v1.2.1 - 2018-07-06

- FCM notifications can now handle `time_to_live`, `collapse_key`, `restricted_package_name`
  and `dry_run` keys

## v1.2.0 - 2018-05-25

- Support for APNS JWT configuration
- Bump `kadabra` dependency to `v0.4.2`

## v1.1.6 - 2018-02-12

- Relax `gen_stage` dependency to `~> 0.12`
- Bump `kadabra` dependency to `v0.3.7`

## v1.1.5 - 2018-01-31

- Fix: relax `httpoison` dependency to allow `0.x` or `1.0`

## v1.1.4 - 2018-01-03

- Fix: `:on_response` callbacks spawned as supervised task instead of running
  in the `Worker` process
- Fix: ADM token refresh failure returns updated notification instead of
  error tuple

## v1.1.3 - 2017-12-23

- More robust FCM/APNS backpressure
- Bumped minimum Kadabra version to `v0.3.6`

## v1.1.2 - 2017-12-04

- Auto-restart connections if max stream ID is reached
- FCM/APNS Workers now use GenStage to queue pending pushes
- Bumped minimum Kadabra version to `v0.3.5`

## v1.1.1 - 2017-10-31

- Bumped minimum Kadabra version to `v0.3.4`

## v1.1.0 - 2017-10-01

- Minimum requirements now Elixir v1.4 and OTP 19.2 (Kadabra bumped
  to `v0.3.0`)
- Startup worker configs. Create a functions that return config
  structs and specify them your `config.exs` with

```elixir
config :pigeon, workers: [
  {YourApp.Pigeon, :apns_config},
  {YourApp.Pigeon, :fcm_config},
  {YourApp.Pigeon, :adm_config},
  ...
]
```

**APNS**

- `APNS.Config.config/1` renamed to `APNS.Config.new/1`
- `APNS.push/2` tagged tuples done away with in favor of a `:response` key on
  the notification.
- Override push server endpoint with `:uri` option in `APNS.Config.new/1`
- `:use_2197` renamed to `:port`
- `:uri` config option for overriding push server endpoint
- `:reconnect` now false by default

**FCM**

- `NotificationResponse` done away with in favor of a `:response` key
  on `Notification`
- Override push server endpoint with `:uri` and `:port` options
  in `FCM.Config.new/1`
- `:uri` and `:port` config options for overriding push server endpoint

**ADM**

- `ADM.Config.config/1` renamed to `ADM.Config.new/1`
- `ADM.push/2` tagged tuples done away with in favor of a `:response` key on
  the notification.
- `ADM.start_connection/1` and `ADM.stop_connection/1` added

## v1.0.4 - 2017-09-06

- Fix: removed connection pinging from FCM.Worker (`:ping_period` option
  left in FCM config to not break API)

## v1.0.3 - 2017-08-03

- Fixed proper handling of large FCM push batches

## v1.0.2 - 2017-08-01

- Fixed FCM infinite `GOAWAY session_timed_out` loop

## v1.0.1 - 2017-08-01

- Configurable `:ping_period` for FCM connections

## v1.0.0 - 2017-07-30

- GCM migrated to FCM API (http2)
- `GCM` modules renamed to `FCM`
- Set priority of FCM notifications
- `APNSWorker` and `ADMWorker` renamed to `APNS.Worker` and `ADM.Worker`
- Disable auto-reconnect for APNS workers with `reconnect: false`
- Removed Chatterbox http2 client adapter

## Previous versions

- See the CHANGELOG.md [in the v0.13 branch](https://github.com/codedge-llc/pigeon/blob/v0.13/CHANGELOG.md)
