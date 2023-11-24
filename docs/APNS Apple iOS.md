# APNS (Apple iOS)

## Usage

1. Set your environment variables. See below for setting up your certificate and key.

   ```elixir
   config :pigeon, :apns,
     apns_default: %{
       cert: "cert.pem",
       key: "key_unencrypted.pem",
       mode: :dev
     }
   ```

   This config sets up a `default` socket connection to send to APNS servers. `cert` and `key` can be any of the following:

   - Static file path
   - Full-text string of the file contents (useful for environment variables)
   - `{:my_app, "certs/cert.pem"}` (indicates path relative to the `priv` folder of the given application)

   Alternatively, you can use token based authentication:

   ```elixir
   config :pigeon, :apns,
     apns_default: %{
       key: "AuthKey.p8",
       key_identifier: "ABC1234567",
       team_id: "DEF8901234",
       mode: :dev
     }
   ```

   - `:key` - Created and downloaded via your developer account. Like `:cert` this can be a file path, file contents string or tuple
   - `:key_identifier` - The 10-character key identifier associated with `:key`, obtained from your developer account
   - `:team_id` - Your 10-character Team ID, obtained from your developer account

2. Create a notification packet. **Note: Your push topic is generally the app's bundle identifier.**

   ```elixir
   n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic (optional)")
   ```

3. Send the packet. Pushes are synchronous and return the notification with an
   updated `:response` key.

   ```elixir
   Pigeon.APNS.push(n)
   ```

## Notification Struct

The contents of `payload` is what will be received on the iOS device. If
updating this field directly, use strings for your keys. It is recommended
to use the convenience functions defined in _Notifications with Custom Data_.
`expiration` is a UNIX epoch date in seconds (UTC). Passing a value of
`0` expires the notification immediately and Apple will not attempt to
redeliver it.

```elixir
%Pigeon.APNS.Notification{
  collapse_id: String.t() | nil,
  device_token: String.t() | nil,
  expiration: non_neg_integer | nil,
  id: String.t() | nil,
  payload: %{String.t() => String.t()},
  response: atom,
  topic: String.t() | nil
}
```

## Generating Your Certificate and Key .pem

1. In Keychain Access, right-click your push certificate and
   select _"Export..."_
2. Export the certificate as `cert.p12`
3. Click the dropdown arrow next to the certificate, right-click the private
   key and select _"Export..."_
4. Export the private key as `key.p12`
5. From a shell, convert the certificate.

   ```
   openssl pkcs12 -clcerts -nokeys -out cert.pem -in cert.p12
   ```

6. Convert the key. Be sure to set a PEM pass phrase here. The pass phrase must be 4 or more characters in length or this will not work. You will need that pass phrase added here in order to remove it in the next step.

   ```
   openssl pkcs12 -nocerts -out key.pem -in key.p12
   ```

7. Remove the PEM pass phrase from the key.

   ```
   openssl rsa -in key.pem -out key_unencrypted.pem
   ```

8. `cert.pem` and `key_unencrypted.pem` can now be used as the cert and key
   in `Pigeon.push`, respectively. Set them in your `config.exs`

## Notifications with Custom Data

Notifications can contain additional information in `payload`. (e.g. setting
badge counters or defining custom sounds)

```elixir
import Pigeon.APNS.Notification
n = Pigeon.APNS.Notification.new("message", "device token", "push topic")
|> put_badge(5)
|> put_sound("default")
|> put_content_available
|> put_mutable_content
|> put_category("category")
|> put_interruption_level("time-sensitive")
```

Using a more complex `alert` dictionary?

```elixir
n
|> put_alert(%{
  "title" => "alert title",
  "body" => "alert body"
})
```

Define custom payload data like so:

```elixir
n
|> put_custom(%{"your-custom-key" => %{
    "custom-value" => 500
  }})
```

## Custom Worker Connections

Multiple APNS worker connections can be configured simultaneously.
Useful for supporting multiple apps and/or certificates at once.

```elixir
config :pigeon, :apns,
  default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  },
  custom_worker: %{
    cert: "another_cert.pem",
    key: "another_key_unencrypted.pem",
    mode: :prod
  }
```

Send pushes with a `to` option in your second parameter.

```elixir
n = Pigeon.APNS.Notification.new("message", "device token", "push topic")
Pigeon.APNS.push(n, to: :custom_worker)
```

You can also start connections manually.

```elixir
iex> {:ok, pid} = Pigeon.APNS.start_connection(cert: "cert.pem",
...> key: "key.pem", mode: :dev)
iex> Pigeon.APNS.push(notif, to: pid)

iex> Pigeon.APNS.start_connection(cert: "cert.pem", key: "key.pem",
...> mode: :dev, name: :custom)
iex> Pigeon.APNS.push(notif, to: :custom)
```

## Asynchronous Pushing

1.  Pass an `on_response` option with an anonymous function in your
    second parameter.

        ```elixir
        n = Pigeon.APNS.Notification.new("message", "device token", "push topic")
        Pigeon.APNS.push(n, on_response: fn(x) -> IO.inspect(x) end)
        ```

2.  Responses return a notification with an updated `:response` key.
    You could handle responses like so:

    ```elixir
    handler = fn(%Pigeon.APNS.Notification{response: response}) ->
      case response do
        :success ->
          Logger.debug "Push successful!"
        :bad_device_token ->
          Logger.error "Bad device token!"
        _error ->
          Logger.error "Some other error happened."
      end
    end

    n = Pigeon.APNS.Notification.new("message", "device token", "push topic")
    Pigeon.APNS.push(n, on_response: handler)
    ```

## Error Responses

_Taken from [APNS Provider API](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html#//apple_ref/doc/uid/TP40008194-CH11-SW17)_

| Reason                             | Description                                                                                                                                                                                 |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `:bad_collapse_id`                 | The collapse identifier exceeds the maximum allowed size                                                                                                                                    |
| `:bad_device_token`                | The specified device token was bad. Verify that the request contains a valid token and that the token matches the environment.                                                              |
| `:bad_expiration_date`             | The apns-expiration value is bad.                                                                                                                                                           |
| `:bad_message_id`                  | The apns-id value is bad.                                                                                                                                                                   |
| `:bad_priority`                    | The apns-priority value is bad.                                                                                                                                                             |
| `:bad_topic`                       | The apns-topic was invalid.                                                                                                                                                                 |
| `:device_token_not_for_topic`      | The device token does not match the specified topic.                                                                                                                                        |
| `:duplicate_headers`               | One or more headers were repeated.                                                                                                                                                          |
| `:idle_timeout`                    | Idle time out.                                                                                                                                                                              |
| `:missing_device_token`            | The device token is not specified in the request :path. Verify that the :path header contains the device token.                                                                             |
| `:missing_topic`                   | The apns-topic header of the request was not specified and was required. The apns-topic header is mandatory when the client is connected using a certificate that supports multiple topics. |
| `:payload_empty`                   | The message payload was empty.                                                                                                                                                              |
| `:topic_disallowed`                | Pushing to this topic is not allowed.                                                                                                                                                       |
| `:bad_certificate`                 | The certificate was bad.                                                                                                                                                                    |
| `:bad_certificate_environment`     | The client certificate was for the wrong environment.                                                                                                                                       |
| `:expired_provider_token`          | The provider token is stale and a new token should be generated.                                                                                                                            |
| `:forbidden`                       | The specified action is not allowed.                                                                                                                                                        |
| `:invalid_provider_token`          | The provider token is not valid or the token signature could not be verified.                                                                                                               |
| `:invalid_push_type`               | The apns-push-type value is invalid.                                                                                                                                                        |
| `:missing_provider_token`          | No provider certificate was used to connect to APNs and Authorization header was missing or no provider token was specified.                                                                |
| `:bad_path`                        | The request contained a bad :path value.                                                                                                                                                    |
| `:method_not_allowed`              | The specified :method was not POST.                                                                                                                                                         |
| `:expired_token`                   | The device token has expired.                                                                                                                                                     |
| `:unregistered`                    | The device token is inactive for the specified topic.                                                                                                                                       |
| `:payload_too_large`               | The message payload was too large. The maximum payload size is 4096 bytes.                                                                                                                  |
| `:too_many_provider_token_updates` | The provider token is being updated too often.                                                                                                                                              |
| `:too_many_requests`               | Too many requests were made consecutively to the same device token.                                                                                                                         |
| `:internal_server_error`           | An internal server error occurred.                                                                                                                                                          |
| `:service_unavailable`             | The service is unavailable.                                                                                                                                                                 |
| `:shutdown`                        | The server is shutting down.                                                                                                                                                                |
