# GCM (Android)

## Usage

1. Set your environment variables.

    ```elixir
  config :pigeon, :gcm,
    key: "your_gcm_key_here"
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
  
## Sending to Multiple Registration IDs

Pass in a list of registration IDs, as many as you want. IDs will automatically be chunked into sets of 1000 before sending the push (as per GCM guidelines).

  ```elixir
  msg = %{ "body" => "your message" }
  n = Pigeon.GCM.Notification.new(["first ID", "second ID"], msg)
  ```

## Notification Struct

When using `Pigeon.GCM.Notification.new/2`, `message_id` and `updated_registration` will always be `nil`. These keys are set in the response callback. `registration_id` can either be a single string or a list of strings.

  ```elixir
  %Pigeon.GCM.Notification{
      payload: %{},
      message_id: nil,
      registration_id: nil,
      updated_registration_id: nil
  }
  ```

## Notifications with Custom Data

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
 
## Handling Push Responses

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
  
## Error Responses

*Slightly modified from [GCM Server Reference](https://developers.google.com/cloud-messaging/http-server-ref#error-codes)*

|Reason                           |Description                  |
|---------------------------------|-----------------------------|
|`:missing_registration`          |Missing Registration Token   |
|`:invalid_registration`          |Invalid Registration Token   |
|`:not_registered`                |Unregistered Device          |
|`:invalid_package_name`          |Invalid Package Name         |
|`:authentication_error`          |Authentication Error         |
|`:mismatch_sender_id`            |Mismatched Sender            |
|`:invalid_json`                  |Invalid JSON                 |
|`:message_too_big`               |Message Too Big              |
|`:invalid_data_key`              |Invalid Data Key             |
|`:invalid_ttl`                   |Invalid Time to Live         |
|`:unavailable`                   |Timeout                      |
|`:internal_server_error`         |Internal Server Error        |
|`:device_message_rate_exceeded`  |Message Rate Exceeded        |
|`:topics_message_rate_exceeded`  |Topics Message Rate Exceeded |
|`:unknown_error`                 |Unknown Error                |

