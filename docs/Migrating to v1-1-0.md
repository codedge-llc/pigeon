# Migrating to v1.1.0

## General
- Kadabra has been bumped to `v0.3.1`. Minimum requirements are now Elixir
  v1.4 / OTP 19.2
- Pushes are now synchronous by default for all services. For async
  functionality, pass an `:on_response` callback as an option with `push/2`.
- `{:ok, notif}`/`{:error, reason, notif}` response tuples have been replaced
  with a `:reponse` key on the notification.

## APNS
- `APNS.Config.config/1` has been renamed to `APNS.Config.new/1`
- `[%APNS.Notification{}, ...]` is now returned instead of
  `%{ok: [...], error: [...]}` on synchronous APNS pushes.
- `:use_2197` config option has been replaced with `:port`. APNS servers still
  only accept `443` and `2197`, but other ports may be useful for test servers.
* `:reconnect` is now false by default
- `push/3` removed in favor of `push/2` with options

## FCM
- `FCM.NotificationResponse` has been dropped in favor of returning
  `FCM.Notification` structs with updated response keys. The old
  `NotificationResponse` keys`:ok`, `:remove`,
  `:update` and `:retry` can be similarly accessed on `Notification` with the
  helper functions `success?/1`, `remove?/1`, `update?/1` and `retry?/1`.
- `push/3` removed in favor of `push/2` with options

## ADM
- `ADM.Config.config/1` has been renamed to `ADM.Config.new/1`
- `push/3` removed in favor of `push/2` with options
