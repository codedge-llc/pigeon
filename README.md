![logo](https://raw.githubusercontent.com/codedge-llc/pigeon/master/docs/logo.png)
> HTTP2-compliant wrapper for sending iOS and Android push notifications.

[![Build Status](https://travis-ci.org/codedge-llc/pigeon.svg?branch=master)](https://travis-ci.org/codedge-llc/pigeon)
[![Coverage Status](https://coveralls.io/repos/github/codedge-llc/pigeon/badge.svg)](https://coveralls.io/github/codedge-llc/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon)
[![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)

## Installation
*Requires Elixir 1.4/OTP 19.2 or later.*
 
Add pigeon and kadabra as `mix.exs` dependencies:
  ```elixir
  def deps do
    [
      {:pigeon, "~> 1.2.3"},
      {:kadabra, "~> 0.4.3"}
    ]
  end
  ```

## Quickstart Guides

### Apple iOS (APNS)

1. Add a default worker config to your mix config. See [the detailed docs](https://hexdocs.pm/pigeon/apns-apple-ios.html) for setting up your certificate and key.

    ```elixir
    config :pigeon, :apns,
      apns_default: %{
        cert: "cert.pem",
        key: "key_unencrypted.pem",
        mode: :dev
      }
    ```

    This config sets up a default connection to APNS servers. `cert` and `key` can be any of the following:
    * Static file path
    * Full-text string of the file contents
    * `{:my_app, "certs/cert.pem"}` (indicates path relative to the `priv` folder of the given application)

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

    * `:key` - Created and downloaded via your developer account. Like `:cert` this can be a file path, file contents string or tuple
    * `:key_identifier` - The 10-character key identifier associated with `:key`, obtained from your developer account
    * `:team_id` - Your 10-character Team ID, obtained from your developer account

2. Create a notification packet. **Note: Your push topic is generally the app's bundle identifier.**

    ```elixir
    iex> n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic (optional)")
    ```

3. Send the packet. Pushes are synchronous and return the notification with an
   updated `:response` key.

    ```elixir
    iex> Pigeon.APNS.push(n)
    %Pigeon.APNS.Notification{device_token: "your device token",
     expiration: nil, id: "963B9FDA-EA60-E869-AAB5-9C88C8E7396B",
     payload: %{"aps" => %{"alert" => "your message"}}, response: :success,
     topic: "your push topic"}
    
    # Add an `:on_response` callback for async pushes.
    iex> Pigeon.APNS.push(n, on_response: fn(x) -> IO.inspect(x) end)
    :ok
    ```

Additional documentation: [APNS (Apple iOS)](https://hexdocs.pm/pigeon/apns-apple-ios.html)

### Android (FCM)

Looking for GCM? Try `v0.13` or earlier.

1. Add a default worker config to your mix config.

    ```elixir
    config :pigeon, :fcm,
      fcm_default: %{
        key: "your_fcm_key_here"
      }
    ```

2. Create a notification packet. FCM notifications support

    ```elixir
    iex> msg = %{ "body" => "your message" }
    iex> n = Pigeon.FCM.Notification.new("your device registration ID", msg)
    ```
 
3. Send the packet. Pushes are synchronous and return the notification with
   updated `:status` and `:response` keys. If `:status` is success, `:response`
   will contain a keyword list of individual registration ID responses.
   

    ```elixir
    iex> Pigeon.FCM.push(n)
    %Pigeon.FCM.Notification{message_id: "0:1512580747839227%8911a9178911a917",
     payload: %{"notification" => %{"body" => "your message"}}, priority: :normal,
     registration_id: "your device registration ID",
     response: [success: "your device registration ID"],
     status: :success}
    
    # Add an `:on_response` callback for async pushes.
    iex> Pigeon.FCM.push(n, on_response: fn(x) -> IO.inspect(x) end)
    :ok
    ```

Additional documentation: [FCM (Android)](https://hexdocs.pm/pigeon/fcm-android.html)

### Amazon Android (ADM)

1. Add a default worker config to your mix config.

    ```elixir
    config :pigeon, :adm,
      adm_default: %{
        client_id: "your_oauth2_client_id_here",
        client_secret: "your_oauth2_client_secret_here"
      }
    ```

2. Create a notification packet.

    ```elixir
    iex> msg = %{ "body" => "your message" }
    iex> n = Pigeon.ADM.Notification.new("your device registration ID", msg)
    ```

3. Send the packet.

    ```elixir
    iex> Pigeon.ADM.push(n)
    %Pigeon.ADM.Notification{consolidation_key: nil,
     expires_after: 604800, md5: "M13RuG4uDWqajseQcCiyiw==",
     payload: %{"data" => %{"body" => "your message"}},
     registration_id: "your device registration ID",
     response: :success, updated_registration_id: nil}
     
    # Add an `:on_response` callback for async pushes.
    iex> Pigeon.ADM.push(n, on_response: fn(x) -> IO.inspect(x) end)
    :ok
    ```

Additional documentation: [ADM (Amazon Android)](https://hexdocs.pm/pigeon/adm-amazon-android.html)

