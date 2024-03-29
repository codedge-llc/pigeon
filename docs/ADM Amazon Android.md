# ADM (Amazon Android)

## Usage

1. Set your environment variables.

   ```elixir
   config :pigeon, :adm,
     client_id: "your_oauth2_client_id_here",
     client_secret: "your_oauth2_client_secret_here"
   ```

2. Create a notification packet.

   ```elixir
   msg = %{ "body" => "your message" }
   n = Pigeon.ADM.Notification.new("your device registration ID", msg)
   ```

3. Send the packet.

   ```elixir
   Pigeon.ADM.push(n)
   ```

## Handling Push Responses

1. Pass an optional anonymous function as your second parameter.

   ```elixir
   data = %{ message: "your message" }
   n = Pigeon.ADM.Notification.new("device registration ID", data)
   Pigeon.ADM.push(n, on_response: fn(x) -> IO.inspect(x) end)
   ```

2. Responses return a notification with an updated `:response` key.
   You could handle responses like so:

   ```elixir
   on_response_handler = fn(x) ->
     case x.response do
       :success ->
         # Push successful
         :ok
       :update ->
         new_reg_id = x.updated_registration_id
         # Update the registration ID in the database
       :invalid_registration_id ->
         # Remove the bad ID from the database
       :unregistered ->
         # Remove the bad ID from the database
       error ->
         # Handle other errors
     end
   end

   data = %{ message: "your message" }
   n = Pigeon.ADM.Notification.new("your registration id", data)
   Pigeon.ADM.push(n, on_response: on_response_handler)
   ```

## Error Responses

_Taken from [Amazon Device Messaging docs](https://developer.amazon.com/public/apis/engage/device-messaging/tech-docs/06-sending-a-message)_

| Reason                       | Description                      |
| ---------------------------- | -------------------------------- |
| `:invalid_registration_id`   | Invalid Registration Token       |
| `:invalid_data`              | Bad format JSON data             |
| `:invalid_consolidation_key` | Invalid Consolidation Key        |
| `:invalid_expiration`        | Invalid expiresAfter value       |
| `:invalid_checksum`          | Invalid md5 value                |
| `:invalid_type`              | Invalid Type header              |
| `:unregistered`              | App instance no longer available |
| `:access_token_expired`      | Expired OAuth access token       |
| `:message_too_large`         | Data size exceeds 6 KB           |
| `:max_rate_exceeded`         | See Retry-After response header  |
| `:unknown_error`             | Unknown Error                    |
