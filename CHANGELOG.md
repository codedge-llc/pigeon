# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

**Changed**

- Use Mint for all HTTP/1 and HTTP/2 connections. This replaces the use of `:httpoison`
  and `:kadabra`. ([#296](https://github.com/codedge-llc/pigeon/pull/296))

**Fixed**

- Return `:permission_denied` FCM error response if missing privileges. ([#290](https://github.com/codedge-llc/pigeon/pull/290))
- Minor documentation fixes. ([#294](https://github.com/codedge-llc/pigeon/pull/294))

## v2.0.1 - 2024-12-28

**Changed**

- Moved older CHANGELOG release notes to their appropriate major version release branches. ([#283](https://github.com/codedge-llc/pigeon/pull/283),
  [#284](https://github.com/codedge-llc/pigeon/pull/284), [#285](https://github.com/codedge-llc/pigeon/pull/285))

**Fixed**

- Elixir 1.18 compilation warnings. ([#281](https://github.com/codedge-llc/pigeon/pull/281))

## v2.0.0 - 2024-12-09

No additional changes since v2.0.0-rc.3. Stable release.

## v2.0.0-rc.3 - 2024-09-10

### Breaking Changes

- `Pigeon.LegacyFCM` has been removed entirely and migrated to [pigeon_legacy_fcm](https://github.com/codedge-llc/pigeon-legacy-fcm) package.
- FCM `service_account_json` config option has been removed and replaced with `:auth`, a [Goth](https://github.com/peburrows/goth) configuration.
  See `Pigeon.FCM` documentation for setup and [#235](https://github.com/codedge-llc/pigeon/pull/235) for more details.

**Changed**

- Bump `goth` dependency to `~> 1.4.3`. ([#252](https://github.com/codedge-llc/pigeon/pull/252))

**Fixed**

- `DispatcherWorker` missing a clause for `{:stop, reason}` in the handle_info function.

## v2.0.0-rc.2 - 2024-01-17

**Fixed**

- Resolve APNS `:too_many_provider_token_updates` by moving token generation into
  `APNS.Token` ([#227](https://github.com/codedge-llc/pigeon/pull/227)).
- Support HTTPoison 2.0. ([#236](https://github.com/codedge-llc/pigeon/pull/236))
- Improve handling of FCM error responses. ([#245](https://github.com/codedge-llc/pigeon/pull/245))
- Fix `DispatcherWorker` missing a clause for `{:stop, reason}` in the init function.
- `APNS.Config` keys now decode properly for PEMs generated with OpenSSL 3. ([#248](https://github.com/codedge-llc/pigeon/pull/248))
- Add `ExpiredToken` as APNS error response. ([#240](https://github.com/codedge-llc/pigeon/pull/240))
- Better handling of APNS token lifecycle between `:dev`/`:prod` environments with the same key identifier. ([#239](https://github.com/codedge-llc/pigeon/pull/239))

## v2.0.0-rc.1 - 2022-06-30

**Fixed**

- Reset stream ID when connection is closed. ([#216](https://github.com/codedge-llc/pigeon/pull/217))

**Changed**

- Bump `goth` dependency to `~> 1.3.0`. ([#224](https://github.com/codedge-llc/pigeon/pull/224))
- Bump minimum supported Elixir version to `1.7`.

## v2.0.0-rc.0 - 2021-07-05

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

## Previous versions

- See the CHANGELOG.md [in the v1.6 branch](https://github.com/codedge-llc/pigeon/blob/v1.6/CHANGELOG.md)
