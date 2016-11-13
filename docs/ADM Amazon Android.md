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
  Pigeon.ADM.push(n, fn(x) -> IO.inspect(x) end)
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
      {:error, :invalid_registration_id, notification} ->
        # Remove the bad ID from the database
      {:error, reason, notification} ->
        # Handle other errors
    end
  end

  data = %{ message: "your message" }
  n = Pigeon.ADM.Notification.new("your registration id", data)
  Pigeon.ADM.push(n, on_response)
  ```

## Error Responses

*Taken from [Amazon Device Messaging docs](https://developer.amazon.com/public/apis/engage/device-messaging/tech-docs/06-sending-a-message)*

|Reason                           |Description                      |
|---------------------------------|---------------------------------|
|`:invalid_registration_id`       |Invalid Registration Token       |
|`:invalid_data`                  |Bad format JSON data             |
|`:invalid_consolidation_key`     |Invalid Consolidation Key        |
|`:invalid_expiration`            |Invalid expiresAfter value       |
|`:invalid_checksum`              |Invalid md5 value                |
|`:invalid_type`                  |Invalid Type header              |
|`:unregistered`                  |App instance no longer available |
|`:access_token_expired`          |Expired OAuth access token       |
|`:message_too_large`             |Data size exceeds 6 KB           |
|`:max_rate_exceeded`             |See Retry-After response header  |
|`:unknown_error`                 |Unknown Error                    |
