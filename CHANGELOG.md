# Changelog

## v0.10.0
* Migrated HTTP/2 client from `chatterbox` to `kadabra`
* Support for ADM (Amazon Android) push
* APNS pushes are now synchronous by default. For async pushes use the new `on_response` option. GCM and ADM will have it in the next major release.
* Bulk APNS pushing
* Handling of multiple APNS worker connections with different configs
* Re-implemented APNS socket pings to keep connections open
* `:invalid_jSON` corrected to `:invalid_json`

## v0.9.2
* Fixed GCM error response atom conversion

## v0.9.1
* Fixed :eaddrinuse error when restarting Pigeon too quickly with :apns_2197 enabled

## v0.9.0
* APNS topic made optional
* APNS `put_mutable_content` helper function added
* GCM can be configured on a per-push basis
* Updated to use Macro.underscore

## v0.8.0
* Implemented Chatterbox as APNS HTTP2 client
* APNS server responses now caught asynchronously
* GCM support for `notification` and `data` payload keys (`Pigeon.GCM.Notification.new` API changes)

## v0.7.0
* APNS cert/key configs can now either be a file path, full-text string, or `{:your_app, "path/to/file.pem"}` (which looks in the `/priv` directory of your app)
* Fixed APNSWorker crash on `:ssl.send/2` timeout
* Better error-handling for invalid APNS configs

## v0.6.0
* `Pigeon.APNS.Notification.new/3` returns `%Pigeon.APNS.Notification{}` struct
* Configure APNS to use SSL port 2197 with `apns_2197: true` in your `config.exs`
* Error feedback symbols converted from `CamelCase` to `snake_case`
* APNS expiration values supported with `expiration` key in `%Pigeon.APNS.Notification{}`

## v0.5.2
* Fixed bug where APNSWorker would hang up if SSL connection failed. Now retries the connection twice more before gracefully shutting down. 

## v0.5.1
* GCM error responses return proper chunk of regstration IDs

## v0.5.0
* `Pigeon.GCM.Notification.new/2` returns `%Pigeon.GCM.Notification{}` struct
* Multiple registration IDs allowed in `Pigeon.GCM.push/2`
* Remove `:gcm_worker`
