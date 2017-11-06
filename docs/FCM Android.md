# FCM (Android)

## Usage

1. Set your environment variables.

    ```elixir
    config :pigeon, :fcm,
      fcm_default: %{
        key: "your_fcm_key_here"
      }
    ```

2. Create a notification packet. 

    ```elixir
    msg = %{ "body" => "your message" }
    n = Pigeon.FCM.Notification.new("your device registration ID", msg)
    ```
 
3. Send the packet. Pushes are synchronous and return the notification with
   updated `:status` and `:response` keys. If `:status` is success, `:response`
   will contain a keyword list of individual registration ID responses.

    ```elixir
    Pigeon.FCM.push(n)
    ```

## Sending to Multiple Registration IDs

Pass in a list of registration IDs, as many as you want.

  ```elixir
  msg = %{ "body" => "your message" }
  n = Pigeon.FCM.Notification.new(["first ID", "second ID"], msg)
  ```

## Notification Struct

  ```elixir
  %Pigeon.FCM.Notification{
      payload: %{...},
      registration_id: String.t | [String.t],
      response: [] | [{atom, String.t}, ...], | atom,
      priority: :normal | :high
  }
  ```

## Notifications with Custom Data

FCM accepts both `notification` and `data` keys in its JSON payload. Set them like so:

  ```elixir
  notification = %{ "body" => "your message" }
  data = %{ "key" => "value" }
  Pigeon.FCM.Notification.new("registration ID", notification, data)
  ```

or

  ```elixir
  Pigeon.FCM.Notification.new("registration ID")
  |> put_notification(%{ "body" => "your message" })
  |> put_data(%{ "key" => "value" })
  ```
 
## Handling Push Responses

1. Pass an optional anonymous function as your second parameter.

    ```elixir
    data = %{message: "your message"}
    n = Pigeon.FCM.Notification.new(data, "device registration ID")
    Pigeon.FCM.push(n, fn(x) -> IO.inspect(x) end)
    {:ok, %Pigeon.FCM.NotificationResponse{...}}
    ```

2. Reponses return the notification with an updated response.

    ```elixir
    on_response = fn(n) ->
      case n.status do
        :success ->
          bad_regids = FCM.Notification.remove?(n)
          to_retry = FCM.Notification.retry?(n)
          # Handle updated regids, remove bad ones, etc
        :unauthorized ->
          # Bad FCM key
        error ->
          # Some other error
      end
    end
    
    data = %{message: "your message"}
    n = Pigeon.FCM.Notification.new("your device token", data)
    Pigeon.FCM.push(n, [on_response: on_response])
    ```

## Error Responses

*Slightly modified from [FCM Server Reference](https://firebase.google.com/docs/cloud-messaging/http-server-ref#error-codes)*

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
