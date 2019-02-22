defmodule Pigeon.FCM.Notification do
  @moduledoc """
  Defines FCM notification struct and convenience constructor functions.
  """

  defstruct collapse_key: nil,
            condition: nil,
            content_available: false,
            dry_run: false,
            message_id: nil,
            mutable_content: false,
            payload: %{},
            priority: :normal,
            registration_id: nil,
            response: [],
            restricted_package_name: nil,
            status: nil,
            time_to_live: 2_419_200

  alias Pigeon.FCM.Notification

  @type t :: %__MODULE__{
          collapse_key: nil | String.t(),
          condition: nil | String.t(),
          content_available: boolean,
          dry_run: boolean,
          message_id: nil | String.t(),
          mutable_content: boolean,
          payload: map,
          priority: :normal | :high,
          registration_id: String.t() | [String.t()],
          response: [] | [regid_response, ...],
          restricted_package_name: nil | String.t(),
          status: status | nil,
          time_to_live: non_neg_integer
        }

  @typedoc ~S"""
  Status of FCM request

  - `:success` - Notification was processed successfully
  - `:timeout` - Worker did not respond within timeout. This is likely an
     internal error
  - `:unauthorized` - Bad FCM key
  - `:malformed_json` - Push payload was invalid JSON
  - `:internal_server_error` - FCM server encountered an error while trying
     to process the request
  - `:unavailable` - FCM server couldn't process the request in time
  """
  @type status ::
          :success
          | :timeout
          | :unauthorized
          | :malformed_json
          | :internal_server_error
          | :unavailable

  @typedoc ~S"""
  FCM push response for individual registration IDs

  - `{:success, "reg_id"}` - Push was successfully sent
  - `{:update, {"reg_id", "new_reg_id"}}` - Push successful but user should
    use new registration ID for future pushes
  - `{regid_error_response, "reg_id"}` - Push attempted but server responded
    with error
  """
  @type regid_response ::
          {:success, binary}
          | {regid_error_response, binary}
          | {:update, {binary, binary}}

  @type regid_error_response ::
          :device_message_rate_exceeded
          | :internal_server_error
          | :invalid_apns_credential
          | :invalid_data_key
          | :invalid_package_name
          | :invalid_parameters
          | :invalid_registration
          | :invalid_ttl
          | :message_too_big
          | :missing_registration
          | :mismatch_sender_id
          | :not_registered
          | :topics_message_rate_exceeded
          | :unavailable
          | :unknown_error

  @chunk_size 1_000

  @doc """
  Creates `FCM.Notification` struct with device registration IDs and optional
  notification and data payloads.

  ## Examples

      iex> Pigeon.FCM.Notification.new("reg ID")
      %Pigeon.FCM.Notification{
        payload: %{},
        registration_id: "reg ID",
        priority: :normal
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"})
      %Pigeon.FCM.Notification{
        payload: %{"notification" => %{"body" => "test message"}},
        registration_id: "reg ID",
        priority: :normal
      }

      iex> Pigeon.FCM.Notification.new("reg ID", %{"body" => "test message"},
      ...> %{"key" => "value"})
      %Pigeon.FCM.Notification{
        payload: %{
          "data" => %{"key" => "value"},
          "notification" => %{"body" => "test message"}
        },
        registration_id: "reg ID",
        priority: :normal
      }

      iex> regids = Enum.map(0..1_499, fn(_x) -> "reg ID" end)
      iex> [n1 | [n2]] = Pigeon.FCM.Notification.new(regids,
      ...> %{"body" => "test message"}, %{"key" => "value"})
      iex> Enum.count(n1.registration_id)
      1000
      iex> Enum.count(n2.registration_id)
      500
  """
  def new(registration_ids, notification \\ %{}, data \\ %{})

  def new(reg_id, notification, data) when is_binary(reg_id) do
    %Pigeon.FCM.Notification{registration_id: reg_id}
    |> put_notification(notification)
    |> put_data(data)
  end

  def new(reg_ids, notification, data) when length(reg_ids) < 1001 do
    %Pigeon.FCM.Notification{registration_id: reg_ids}
    |> put_notification(notification)
    |> put_data(data)
  end

  def new(reg_ids, notification, data) do
    reg_ids
    |> chunk(@chunk_size, @chunk_size, [])
    |> Enum.map(&new(&1, notification, data))
    |> List.flatten()
  end

  defp chunk(collection, chunk_size, step, padding) do
    if Kernel.function_exported?(Enum, :chunk_every, 4) do
      Enum.chunk_every(collection, chunk_size, step, padding)
    else
      Enum.chunk(collection, chunk_size, step, padding)
    end
  end

  @doc """
  Updates `"data"` key in push payload.

  This parameter specifies the custom key-value pairs of the message's payload.

  For example, with data:{"score":"3x1"}:

  On iOS, if the message is sent via APNs, it represents the custom data fields.
  If it is sent via FCM connection server, it would be represented as key value
  dictionary in AppDelegate application:didReceiveRemoteNotification:.

  On Android, this would result in an intent extra named score with the string
  value 3x1.

  The key should not be a reserved word ("from" or any word starting with
  "google" or "gcm"). Do not use any of the words defined in this table
  (such as collapse_key).

  Values in string types are recommended. You have to convert values in objects
  or other non-string data types (e.g., integers or booleans) to string.

  ## Examples

      iex> put_data(%Pigeon.FCM.Notification{}, %{"key" => 1234})
      %Pigeon.FCM.Notification{
        payload: %{"data" => %{"key" => 1234}},
        registration_id: nil
      }
  """
  def put_data(n, data), do: update_payload(n, "data", data)

  @doc """
  Updates `"notification"` key in push payload.

  This parameter specifies the predefined, user-visible key-value pairs of the
  notification payload. See Notification payload support for detail. For more
  information about notification message and data message options, see
  [Message types](https://firebase.google.com/docs/cloud-messaging/concept-options#notifications_and_data_messages).
  If a notification payload is provided, or the content_available option is set
  to true for a message to an iOS device, the message is sent through APNs,
  otherwise it is sent through the FCM connection server.

  ## Examples

      iex> put_notification(%Pigeon.FCM.Notification{},
      ...> %{"body" => "message"})
      %Pigeon.FCM.Notification{
        payload: %{"notification" => %{"body" => "message"}},
        registration_id: nil
      }
  """
  def put_notification(n, notification),
    do: update_payload(n, "notification", notification)

  @doc """
  Updates `"priority"` key.

  Sets the priority of the message. Valid values are "normal" and "high." On
  iOS, these correspond to APNs priorities 5 and 10.

  By default, notification messages are sent with high priority, and data
  messages are sent with normal priority. Normal priority optimizes the client
  app's battery consumption and should be used unless immediate delivery is
  required. For messages with normal priority, the app may receive the message
  with unspecified delay.

  When a message is sent with high priority, it is sent immediately, and the app
  can display a notification.

  ## Examples

      iex> put_priority(%Pigeon.FCM.Notification{}, :normal)
      %Pigeon.FCM.Notification{priority: :normal}

      iex> put_priority(%Pigeon.FCM.Notification{}, :high)
      %Pigeon.FCM.Notification{priority: :high}

      iex> put_priority(%Pigeon.FCM.Notification{priority: :normal}, :bad)
      %Pigeon.FCM.Notification{priority: :normal}
  """
  def put_priority(n, :normal), do: %{n | priority: :normal}
  def put_priority(n, :high), do: %{n | priority: :high}
  def put_priority(n, _), do: n

  @doc """
  Updates `"time_to_live"` key.

  This parameter specifies how long (in seconds) the message should be kept in
  FCM storage if the device is offline. The maximum time to live supported is 4
  weeks, and the default value is 4 weeks. For more information, see
  [Setting the lifespan of a message](https://firebase.google.com/docs/cloud-messaging/concept-options#ttl).

  ## Examples

      iex> put_time_to_live(%Pigeon.FCM.Notification{}, 60 * 60 * 24)
      %Pigeon.FCM.Notification{time_to_live: 86_400}
  """
  def put_time_to_live(n, ttl) when is_integer(ttl),
    do: %{n | time_to_live: ttl}

  @doc """
  Sets `"dry_run"` key to true.

  This parameter, when set to true, allows developers to test a request without
  actually sending a message.

  The default value is false.

  ## Examples

      iex> put_dry_run(%Pigeon.FCM.Notification{})
      %Pigeon.FCM.Notification{dry_run: true}
  """
  def put_dry_run(n), do: %{n | dry_run: true}

  @doc """
  Updates `"collapse_key"` key.

  ## Examples

      iex> put_collapse_key(%Pigeon.FCM.Notification{}, "Updates available")
      %Pigeon.FCM.Notification{collapse_key: "Updates available"}
  """
  def put_collapse_key(n, key) when is_binary(key), do: %{n | collapse_key: key}

  @doc """
  Updates `"restricted_package_name"` key.

  This parameter specifies the package name of the application where the
  registration tokens must match in order to receive the message. (Only affects android)

  ## Examples

      iex> put_restricted_package_name(%Pigeon.FCM.Notification{}, "com.example.app")
      %Pigeon.FCM.Notification{restricted_package_name: "com.example.app"}
  """
  def put_restricted_package_name(n, name) when is_binary(name),
    do: %{n | restricted_package_name: name}

  @doc """
  Updates `"content_available"` key.

  On iOS, use this field to represent content-available in the APNs payload.
  When a notification or message is sent and this is set to true, an inactive
  client app is awoken, and the message is sent through APNs as a silent
  notification and not through the FCM connection server. Note that silent
  notifications in APNs are not guaranteed to be delivered, and can depend on
  factors such as the user turning on Low Power Mode, force quitting the app,
  etc. On Android, data messages wake the app by default. On Chrome, currently
  not supported.

  ## Examples

      iex> put_content_available(%Pigeon.FCM.Notification{}, true)
      %Pigeon.FCM.Notification{content_available: true}
  """
  def put_content_available(n, enabled) when is_boolean(enabled),
    do: %{n | content_available: enabled}

  @doc """
  Updates `"mutable_content"` key.

  Currently for iOS 10+ devices only. On iOS, use this field to represent
  mutable-content in the APNs payload. When a notification is sent and this is
  set to true, the content of the notification can be modified before it is
  displayed, using a Notification Service app extension. This parameter will be
  ignored for Android and web.

  ## Examples

      iex> put_mutable_content(%Pigeon.FCM.Notification{}, true)
      %Pigeon.FCM.Notification{mutable_content: true}
  """
  def put_mutable_content(n, enabled) when is_boolean(enabled),
    do: %{n | mutable_content: enabled}

  @doc """
  Updates `"condition"` key.

  ## Examples

      iex> put_condition(%Pigeon.FCM.Notification{}, "'test' in topics")
      %Pigeon.FCM.Notification{condition: "'test' in topics"}
  """
  def put_condition(n, condition) when is_binary(condition),
    do: %{n | condition: condition}

  defp update_payload(notification, _key, value) when value == %{},
    do: notification

  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)

    %{notification | payload: payload}
  end

  @doc ~S"""
  Returns a list of successful registration IDs

  ## Examples

      iex> n = %Pigeon.FCM.Notification{response: [
      ...> {:success, "regid1"}, {:invalid_registration, "regid2"},
      ...> {:success, "regid3"}, {:update, {"regid4", "new_regid4"}},
      ...> {:not_registered, "regid5"}, {:unavailable, "regid6"}]}
      iex> success?(n)
      ["regid1", "regid3"]
  """
  def success?(%Notification{response: response}) do
    Keyword.get_values(response, :success)
  end

  @doc ~S"""
  Returns a list of registration IDs and their corresponding new ID

  ## Examples

      iex> n = %Pigeon.FCM.Notification{response: [
      ...> {:success, "regid1"}, {:invalid_registration, "regid2"},
      ...> {:success, "regid3"}, {:update, {"regid4", "new_regid4"}},
      ...> {:not_registered, "regid5"}, {:unavailable, "regid6"}]}
      iex> update?(n)
      [{"regid4", "new_regid4"}]
  """
  def update?(%Notification{response: response}) do
    Keyword.get_values(response, :update)
  end

  @doc ~S"""
  Returns a list of registration IDs that should be retried

  ## Examples

      iex> n = %Pigeon.FCM.Notification{response: [
      ...> {:success, "regid1"}, {:invalid_registration, "regid2"},
      ...> {:success, "regid3"}, {:update, {"regid4", "new_regid4"}},
      ...> {:not_registered, "regid5"}, {:unavailable, "regid6"}]}
      iex> retry?(n)
      ["regid6"]
  """
  def retry?(%{response: response}) do
    Keyword.get_values(response, :unavailable)
  end

  @doc ~S"""
  Returns a list of registration IDs that should be removed

  ## Examples

      iex> n = %Pigeon.FCM.Notification{response: [
      ...> {:success, "regid1"}, {:invalid_registration, "regid2"},
      ...> {:success, "regid3"}, {:update, {"regid4", "new_regid4"}},
      ...> {:not_registered, "regid5"}, {:unavailable, "regid6"}]}
      iex> remove?(n)
      ["regid2", "regid5"]
  """
  def remove?(%{response: response}) do
    response
    |> Enum.filter(fn {k, _v} ->
      k == :invalid_registration || k == :not_registered
    end)
    |> Keyword.values()
  end
end

defimpl Pigeon.Encodable, for: Pigeon.FCM.Notification do
  def binary_payload(notif) do
    encode_requests(notif)
  end

  @doc false
  def encode_requests(%{registration_id: regid} = notif)
      when is_binary(regid) do
    encode_requests(%{notif | registration_id: [regid]})
  end

  def encode_requests(%{registration_id: regid} = notif) when is_list(regid) do
    regid
    |> recipient_attr()
    |> Map.merge(notif.payload)
    |> encode_attr("priority", to_string(notif.priority))
    |> encode_attr("time_to_live", notif.time_to_live)
    |> encode_attr("collapse_key", notif.collapse_key)
    |> encode_attr("restricted_package_name", notif.restricted_package_name)
    |> encode_attr("dry_run", notif.dry_run)
    |> encode_attr("content_available", notif.content_available)
    |> encode_attr("mutable_content", notif.mutable_content)
    |> Poison.encode!()
  end

  defp encode_attr(map, _key, nil), do: map

  defp encode_attr(map, key, val) do
    Map.put(map, key, val)
  end

  defp recipient_attr([regid]), do: %{"to" => regid}

  defp recipient_attr(regid) when is_list(regid),
    do: %{"registration_ids" => regid}
end
