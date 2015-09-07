[![Hex.pm](http://img.shields.io/hexpm/v/pigeon.svg)](https://hex.pm/packages/pigeon) [![Hex.pm](http://img.shields.io/hexpm/dt/pigeon.svg)](https://hex.pm/packages/pigeon)
# Pigeon
iOS push notifications for Elixir

## Usage
1. Create a notification packet with `n = Pigeon.Notification.new(message, device_token)`
2. Send the packet with `Pigeon.push(n, uri, "your_push_cert.pem", "your_push_key.pem")` where `uri` can be one of the following:
    * `Pigeon.apple_development_gateway_uri`
    * `Pigeon.apple_production_gateway_uri`

## Generating Your Certificate and Key .pem
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
   pkcs12 -nocerts -out key.pem -in key.p12
   ```

7. Remove the password from the key.
   ```
   openssl rsa -in key.pem -out key_unencrypted.pem
   ```
   
8. `cert.pem` and `key_unencrypted.pem` can now be used as the cert and key in `Pigeon.push`, respectively.
