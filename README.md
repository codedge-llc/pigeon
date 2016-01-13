[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
# Pigeon
iOS and Android push notifications for Elixir

## Installation
Add pigeon as a `mix.exs` dependency:
  ```
  def deps do
    [{:pigeon, "~> 0.2.0"}]
  end
  ```
  
## GCM (Android)
### Usage
1. Create a notification packet. 
  ```
  data = %{ key1: "value1", key2: "value2" }
  n = Pigeon.GCM.Notification.new(data, "your device token (A.K.A. registration ID)")
  ```
 
2. Send the packet.
  ```
  Pigeon.GCM.push(n)
  ```

## APNS (Apple iOS)
### Usage
1. Create a notification packet.
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token")
  ```
  
2. Create an APNS connection, where `mode` can be either `:dev` or `:prod`
  ```
  c = Pigeon.APNS.Connection.new(mode, "your_push_cert.pem", "your_push_key.pem")
  ``` 
  
3. Send the packet.
  ```
  Pigeon.APNS.push(n, c)
  ```
  
### Generating Your Certificate and Key .pem
1. In Keychain Access, right-click your push certificate and select _"Export..."_
2. Export the certificate as `cert.p12`
3. Click the dropdown arrow next to the certificate, right-click the private key and select _"Export..."_
4. Export the private key as `key.p12`
5. From a shell, convert the certificate.
   ```
   openssl pkcs12 -clcerts -nokeys -out cert.pem -in cert.p12`
   ```
   
6. Convert the key.
   ```
   openssl pkcs12 -nocerts -out key.pem -in key.p12
   ```

7. Remove the password from the key.
   ```
   openssl rsa -in key.pem -out key_unencrypted.pem
   ```
   
8. `cert.pem` and `key_unencrypted.pem` can now be used as the cert and key in `Pigeon.push`, respectively.

### Notifications with Custom Data
Notifications can contain additional information for the `aps` key with a map passed as an optional 3rd parameter (e.g. setting badge counters or defining custom sounds)
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", %{
    badge: 5,
    sound: "default"
  })
  ```
  
Or define custom payload data with an optional 4th parameter:
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", %{}, %{
    your-custom-key: %{
      custom-value: 500
    }
  })
  ```
