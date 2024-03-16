# Changelog

## Unreleased

**Changed**

- Bump `goth` dependency to `~> 1.4.3`. ([#252](https://github.com/codedge-llc/pigeon/pull/252))

## v2.0.0-rc.2

**Fixed**

- Resolve APNS `:too_many_provider_token_updates` by moving token generation into
  `APNS.Token` ([#227](https://github.com/codedge-llc/pigeon/pull/227)).
- Support HTTPoison 2.0. ([#236](https://github.com/codedge-llc/pigeon/pull/236))
- Improve handling of FCM error responses. ([#245](https://github.com/codedge-llc/pigeon/pull/245))
- Fix `DispatcherWorker` missing a clause for `{:stop, reason}` in the init function.
- `APNS.Config` keys now decode properly for PEMs generated with OpenSSL 3. ([#248](https://github.com/codedge-llc/pigeon/pull/248))
- Add `ExpiredToken` as APNS error response. ([#240](https://github.com/codedge-llc/pigeon/pull/240))
- Better handling of APNS token lifecycle between `:dev`/`:prod` environments with the same key identifier. ([#239](https://github.com/codedge-llc/pigeon/pull/239))

## v2.0.0-rc.1

**Fixed**

- Reset stream ID when connection is closed. ([#216](https://github.com/codedge-llc/pigeon/pull/217))

**Changed**

- Bump `goth` dependency to `~> 1.3.0`. ([#224](https://github.com/codedge-llc/pigeon/pull/224))
- Bump minimum supported Elixir version to `1.7`.

## v2.0.0-rc.0

**Changed**

- Default JSON library set to Jason.
  ([#182](https://github.com/codedge-llc/pigeon/pull/182))
- Pigeon application module moved from `Pigeon` to `Pigeon.Application`.
  ([#183](https://github.com/codedge-llc/pigeon/pull/183))
- Kadabra bumped to v0.5.0, and now a required dependency.
  ([#184](https://github.com/codedge-llc/pigeon/pull/184))
- Sending a list of pushes synchronously now actually sends them one at a time. For production
  workloads, using the async `:on_response` callback is strongly suggested.

**Fixed**

- Removed Elixir 1.11 compile warnings.
  ([#184](https://github.com/codedge-llc/pigeon/pull/184))

**Removed**

- `:certfile` and `:keyfile` are no longer valid options for APNS configurations.
  Instead, read the file before loading (e.g. `cert: File.read!("cert.pem")`)
  ([#183](https://github.com/codedge-llc/pigeon/pull/183))
- `:debug_log` removed.

## v1.6.0

**Added**

- JSON library made configurable. For backwards compatibility, Poison is still a required dependency.
  Override in your `config.exs`.

  ```
  config :pigeon, json_library: Jason
  ```

**Fixed**

- Handle FCM single message_id's on topic pushes.

## v1.5.1

- Added APNS InvalidPushType error ([#172](https://github.com/codedge-llc/pigeon/pull/172)).
- Fixed various typespecs ([#170](https://github.com/codedge-llc/pigeon/pull/170)).

## v1.5.0

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

## v1.4.0

- `apns-push-type` header support for iOS 13. An additional `:push_type` key has been
  added to the `APNS.Notification` struct.

## v1.3.2

- Document workers configuration for run-time configuration of push workers.
- Modify run-time configuration of push workers so that multiple (or no)
  workers may be returned by the startup configuration.

## v1.3.1

- Joken dependency bumped to 2.1

## v1.3.0

- Support for FCM `content_available`, `mutable_content`, and `condition` keys
- Set `priority` of APNS notifications
- Joken dependency bumped to 2.0.1

## v1.2.4

- Fixed ADM handling of connection timeouts

## v1.2.3

- Fixed APNS, FCM and ADM error response parse crashes. Error responses not
  listed in the documentation are returned as `:unknown_error`

## v1.2.2

- Fixed APNS handling of notification `expiration`
- Added APNS support for `collapse_id`

## v1.2.1

- FCM notifications can now handle `time_to_live`, `collapse_key`, `restricted_package_name`
  and `dry_run` keys

## v1.2.0

- Support for APNS JWT configuration
- Bump `kadabra` dependency to `v0.4.2`

## v1.1.6

- Relax `gen_stage` dependency to `~> 0.12`
- Bump `kadabra` dependency to `v0.3.7`

## v1.1.5

- Fix: relax `httpoison` dependency to allow `0.x` or `1.0`

## v1.1.4

- Fix: `:on_response` callbacks spawned as supervised task instead of running
  in the `Worker` process
- Fix: ADM token refresh failure returns updated notification instead of
  error tuple

## v1.1.3

- More robust FCM/APNS backpressure
- Bumped minimum Kadabra version to `v0.3.6`

## v1.1.2

- Auto-restart connections if max stream ID is reached
- FCM/APNS Workers now use GenStage to queue pending pushes
- Bumped minimum Kadabra version to `v0.3.5`

## v1.1.1

- Bumped minimum Kadabra version to `v0.3.4`

## v1.1.0

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

## v1.0.4

- Fix: removed connection pinging from FCM.Worker (`:ping_period` option
  left in FCM config to not break API)

## v1.0.3

- Fixed proper handling of large FCM push batches

## v1.0.2

- Fixed FCM infinite `GOAWAY session_timed_out` loop

## v1.0.1

- Configurable `:ping_period` for FCM connections

## v1.0.0

- GCM migrated to FCM API (http2)
- `GCM` modules renamed to `FCM`
- Set priority of FCM notifications
- `APNSWorker` and `ADMWorker` renamed to `APNS.Worker` and `ADM.Worker`
- Disable auto-reconnect for APNS workers with `reconnect: false`
- Removed Chatterbox http2 client adapter

## v0.13.0

- Configurable `:ping_period` for APNS connections

## v0.12.1

- Various `chatterbox` client adapter fixes

## V0.12.0

- Configurable `Pigeon.Http2.Client`. Currently supports `kadabra`
  and `chatterbox`
- `kadabra` bumped to `v0.2.0`

## v0.11.0

- APNS workers can be started and referenced with pids and/or atom names
- Fix: Push `:name` option renamed to `:to`
- Fix: GCM/ADM async pushes now use `spawn/1` instead of `Task.async/1`

## v0.10.3

- Fix: cleaned up Elixir v1.4 warnings

## v0.10.2

- Fix: poison dependency version made optionally `~> 2.0 or ~> 3.0`

## v0.10.1

- Fix: kadabra not started

## v0.10.0

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

## v0.9.1

- Fixed :eaddrinuse error when restarting Pigeon too quickly with
  :apns_2197 enabled

## v0.9.0

- APNS topic made optional
- APNS `put_mutable_content` helper function added
- GCM can be configured on a per-push basis
- Updated to use Macro.underscore

## v0.8.0

- Implemented Chatterbox as APNS HTTP2 client
- APNS server responses now caught asynchronously
- GCM support for `notification` and `data` payload keys
  (`Pigeon.GCM.Notification.new` API changes)

## v0.7.0

- APNS cert/key configs can now either be a file path, full-text string,
  or `{:your_app, "path/to/file.pem"}` (which looks in the `/priv` directory
  of your app)
- Fixed APNSWorker crash on `:ssl.send/2` timeout
- Better error-handling for invalid APNS configs

## v0.6.0

- `Pigeon.APNS.Notification.new/3` returns `%Pigeon.APNS.Notification{}` struct
- Configure APNS to use SSL port 2197 with `apns_2197: true` in
  your `config.exs`
- Error feedback symbols converted from `CamelCase` to `snake_case`
- APNS expiration values supported with `expiration` key in
  `%Pigeon.APNS.Notification{}`

## v0.5.2

- Fixed bug where APNSWorker would hang up if SSL connection failed. Now
  retries the connection twice more before gracefully shutting down.

## v0.5.1

- GCM error responses return proper chunk of regstration IDs

## v0.5.0

- `Pigeon.GCM.Notification.new/2` returns `%Pigeon.GCM.Notification{}` struct
- Multiple registration IDs allowed in `Pigeon.GCM.push/2`
- Remove `:gcm_worker`
