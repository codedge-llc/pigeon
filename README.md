[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Deps Status](https://beta.hexfaktor.org/badge/all/github/codedge-llc/pigeon.svg)](https://beta.hexfaktor.org/github/codedge-llc/pigeon)
# Pigeon
HTTP2-compliant wrapper for sending iOS and Android push notifications.

## Installation
**Note: Pigeon's API will likely change until v1.0**

Add pigeon and chatterbox as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 0.8.0"},
      {:chatterbox, github: "joedevivo/chatterbox"}
    ]
  end
  ```
  
After running `mix deps.get`, configure `mix.exs` to start the application automatically.
  ```elixir
  def application do
    [applications: [:pigeon]]
  end
  ```
  
## GCM (Android)
### Usage
1. Set your environment variables.
  ```elixir
  config :pigeon, 
    gcm_key: "your_gcm_key_here"
  ```
  
2. Create a notification packet. 
  ```elixir
  msg = %{ "body" => "your message" }
  n = Pigeon.GCM.Notification.new("your device registration ID", msg)
  ```
 
3. Send the packet.
  ```elixir
  Pigeon.GCM.push(n)
  ```
  
### Sending to Multiple Registration IDs
Pass in a list of registration IDs, as many as you want. IDs will automatically be chunked into sets of 1000 before sending the push (as per GCM guidelines).
  ```elixir
  msg = %{ "body" => "your message" }
  n = Pigeon.GCM.Notification.new(["first ID", "second ID"], msg)
  ```


### Notification Struct
When using `Pigeon.GCM.Notification.new/2`, `message_id` and `updated_registration` will always be `nil`. These keys are set in the response callback. `registration_id` can either be a single string or a list of strings.
```elixir
%Pigeon.GCM.Notification{
    payload: %{},
    message_id: nil,
    registration_id: nil,
    updated_registration_id: nil
}
```

### Notifications with Custom Data
GCM accepts both `notification` and `data` keys in its JSON payload. Set them like so:
```elixir
  notification = %{ "body" => "your message" }
  data = %{ "key" => "value" }
  Pigeon.GCM.Notification.new("registration ID", notification, data)
```

or

```elixir
  	Pigeon.GCM.Notification.new("registration ID")
  	|> put_notification(%{ "body" => "your message" })
  	|> put_data(%{ "key" => "value" })
```
 
## APNS (Apple iOS)
### Usage
1. Set your environment variables. See below for setting up your certificate and key.
  ```elixir
  config :pigeon, 
    apns_mode: :dev,
    apns_cert: "cert.pem",
    apns_key: "key_unencrypted.pem"
    apns_2197: true (optional)
  ```

  `apns_cert` and `apns_key` can either be a static file path, full-text string of the file contents (for environment variables), or a tuple like `{:my_app, "certs/cert.pem"}`,
  which will use a path relative to the `priv` folder of the given application.

2. Create a notification packet. **Note: Your push topic is generally the app's bundle identifier.**
  ```elixir
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  ```
  
  
3. Send the packet.
  ```elixir
  Pigeon.APNS.push(n)
  ```
  
### Notification Struct
The contents of `payload` is what will be received on the iOS device. If updating this field directly, use strings for your keys. It is recommended to use the convenience functions defined in *Notifications with Custom Data*. `expiration` is a UNIX epoch date in seconds (UTC). Passing a value of `0` expires the notification immediately and Apple will not attempt to redeliver it.
```elixir
%Pigeon.APNS.Notification{
    id: nil,
    device_token: nil,
    topic: nil,
    expiration: nil,
    payload: nil
}
```
  
### Generating Your Certificate and Key .pem
1. In Keychain Access, right-click your push certificate and select _"Export..."_
2. Export the certificate as `cert.p12`
3. Click the dropdown arrow next to the certificate, right-click the private key and select _"Export..."_
4. Export the private key as `key.p12`
5. From a shell, convert the certificate.
   ```
   openssl pkcs12 -clcerts -nokeys -out cert.pem -in cert.p12
   ```
   
6. Convert the key. Be sure to set a password here. You will need that password in order to remove it in the next step.
   ```
   openssl pkcs12 -nocerts -out key.pem -in key.p12
   ```

7. Remove the password from the key.
   ```
   openssl rsa -in key.pem -out key_unencrypted.pem
   ```
   
8. `cert.pem` and `key_unencrypted.pem` can now be used as the cert and key in `Pigeon.push`, respectively. Set them in your `config.exs`

### Notifications with Custom Data
Notifications can contain additional information in `payload`. (e.g. setting badge counters or defining custom sounds)
  ```elixir
  import Pigeon.APNS.Notification
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  |> put_badge(5)
  |> put_sound("default")
  |> put_content_available
  |> put_category("category")
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

## Handling Push Responses
### GCM
1. Pass an optional anonymous function as your second parameter. This function will get called on each registration ID assuming some of them were successful.
  ```elixir
  data = %{ message: "your message" }
  n = Pigeon.GCM.Notification.new(data, "device registration ID")
  Pigeon.GCM.push(n, fn(x) -> IO.inspect(x) end)
  ```
  
2. Reponses return a tuple of either `{:ok, notification}` or `{:error, reason, notification}`. You could handle responses like so:
  ```elixir
  on_response = fn(x) ->
    case x do
      {:ok, notification} ->
        # Push successful, check to see if the registration ID changed
        if !is_nil(notification.updated_registration_id) do
          # Update the registration ID in the database
        end
      {:error, :invalid_registration, notification} ->
        # Remove the bad ID from the database
      {:error, reason, notification} ->
        # Handle other errors
    end
  end
  
  data = %{ message: "your message" }
  n = Pigeon.GCM.Notification.new(data, "your device token")
  Pigeon.GCM.push(n, on_response)
  ```
  
Notification structs returned as `{:ok, notification}` will always contain exactly one registration ID for the `registration_id` key. 

For `{:error, reason, notification}` tuples, this key can be one or many IDs depending on the error. `:invalid_registration` will return exactly one, whereas `:authentication_error` and `:internal_server_error` will return up to 1000 IDs (and the callback called for each failed 1000-chunked request).
  
#### Error Responses
*Slightly modified from [GCM Server Reference](https://developers.google.com/cloud-messaging/http-server-ref#error-codes)*

|Reason                           |Description                  |
|---------------------------------|-----------------------------|
|`:missing_registration`          |Missing Registration Token   |
|`:invalid_registration`          |Invalid Registration Token   |
|`:not_registered`                |Unregistered Device          |
|`:invalid_package_name`          |Invalid Package Name         |
|`:authentication_error`          |Authentication Error         |
|`:mismatch_sender_id`            |Mismatched Sender            |
|`:invalid_jSON`                  |Invalid JSON                 |
|`:message_too_big`               |Message Too Big              |
|`:invalid_data_key`              |Invalid Data Key             |
|`:invalid_ttl`                   |Invalid Time to Live         |
|`:unavailable`                   |Timeout                      |
|`:internal_server_error`         |Internal Server Error        |
|`:device_message_rate_exceeded`  |Message Rate Exceeded        |
|`:topics_message_rate_exceeded`  |Topics Message Rate Exceeded |
|`:unknown_error`                 |Unknown Error                |

### APNS
1. Pass an optional anonymous function as your second parameter.
  ```elixir
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  Pigeon.APNS.push(n, fn(x) -> IO.inspect(x) end)
  ```

2. Responses return a tuple of either `{:ok, notification}` or `{:error, reason, notification}`. You could handle responses like so:
  ```elixir
  on_response = fn(x) ->
    case x do
      {:ok, notification} ->
        Logger.debug "Push successful!"
      {:error, :bad_device_token, notification} ->
        Logger.error "Bad device token!"
      {:error, reason, notification} ->
        Logger.error "Some other error happened."
    end
  end

  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  Pigeon.APNS.push(n, on_response)
  ```

#### Error Responses
*Taken from [APNS Provider API](https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/APNsProviderAPI.html#//apple_ref/doc/uid/TP40008194-CH101-SW3)*

|Reason                         |Description                   |
|-------------------------------|------------------------------|
|`:payload_empty`                 |The message payload was empty.|
|`:payload_too_large`             |The message payload was too large. The maximum payload size is 4096 bytes.|
|`:bad_topic`                     |The apns-topic was invalid.|
|`:topic_disallowed`              |Pushing to this topic is not allowed.|
|`:bad_message_id`                |The apns-id value is bad.|
|`:bad_expiration_date`           |The apns-expiration value is bad.|
|`:bad_priority`                  |The apns-priority value is bad.|
|`:missing_device_token`          |The device token is not specified in the request :path. Verify that the :path header contains the device token.|
|`:bad_device_token`              |The specified device token was bad. Verify that the request contains a valid token and that the token matches the environment.|
|`:device_token_not_for_topic`    |The device token does not match the specified topic.|
|`:unregistered`                  |The device token is inactive for the specified topic.|
|`:duplicate_headers`             |One or more headers were repeated.|
|`:bad_certificate_environment`   |The client certificate was for the wrong environment.|
|`:bad_certificate`               |The certificate was bad.|
|`:forbidden`                     |The specified action is not allowed.|
|`:bad_path`                      |The request contained a bad :path value.|
|`:method_not_allowed`            |The specified :method was not POST.|
|`:too_many_requests`             |Too many requests were made consecutively to the same device token.|
|`:idle_timeout`                  |Idle time out.|
|`:shutdown`                      |The server is shutting down.|
|`:internal_server_error`         |An internal server error occurred.|
|`:service_unavailable`           |The service is unavailable.|
|`:missing_topic`                 |The apns-topic header of the request was not specified and was required. The apns-topic header is mandatory when the client is connected using a certificate that supports multiple topics.|
