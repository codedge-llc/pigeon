# Changelog

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
