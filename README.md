[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
# Pigeon
iOS and Android push notifications for Elixir

## Installation
Add pigeon as a `mix.exs` dependency:
  ```
  def deps do
    [{:pigeon, "~> 0.3.0"}]
  end
  ```
  
After running `mix deps.get`, configure `mix.exs` to start the application automatically.
  ```
  def application do
    [applications: [:pigeon]
  end
  ```
  
## GCM (Android)
### Usage
1. Set your environment variables.
  ```
  config :pigeon, 
    gcm_key: "your_gcm_key_here"
  ```
  
2. Create a notification packet. 
  ```
  data = %{ key1: "value1", key2: "value2" }
  n = Pigeon.GCM.Notification.new(data, "your device token (registration ID)")
  ```
 
3. Send the packet.
  ```
  Pigeon.GCM.push(n)
  ```

## APNS (Apple iOS)
### Usage
1. Set your environment environment variables. See below for setting up your certificate and key.
  ```
  config :pigeon, 
    apns_mode: :dev,
    apns_cert: "cert.pem",
    apns_key: "key_unencrypted.pem"
  ```

2. Create a notification packet. **Note: Your push topic is generally the app's bundle identifier.**
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  ```
  
  
3. Send the packet.
  ```
  Pigeon.APNS.push(n)
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
   
8. `cert.pem` and `key_unencrypted.pem` can now be used as the cert and key in `Pigeon.push`, respectively. Set them in your `config.exs`

### Notifications with Custom Data
Notifications can contain additional information for the `aps` key with a map passed as an optional 4th parameter (e.g. setting badge counters or defining custom sounds)
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic", %{
    badge: 5,
    sound: "default"
  })
  ```
  
Or define custom payload data with an optional 5th parameter:
  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic", %{}, %{
    your-custom-key: %{
      custom-value: 500
    }
  })
  ```
