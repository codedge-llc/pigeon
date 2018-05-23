defmodule Pigeon.FCM.Notification do
  @moduledoc """
  Defines FCM notification struct and convenience constructor functions.
  """

  defstruct message_id: nil,
            payload: %{},
            priority: :normal,
            registration_id: nil,
            status: nil,
            response: []

  alias Pigeon.FCM.Notification

  @type t :: %__MODULE__{
          message_id: nil | String.t(),
          payload: %{},
          priority: :normal | :high,
          registration_id: String.t() | [String.t()],
          status: status | nil,
          response: [] | [regid_response, ...]
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
          | :invalid_data_key
          | :invalid_package_name
          | :invalid_paramteres
          | :invalid_registration
          | :invalid_ttl
          | :message_too_big
          | :missing_registration
          | :mismatch_sender_id
          | :not_registered
          | :topics_message_rate_exceeded
          | :unavailable

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
  def encode_requests(%{registration_id: regid} = notification)
      when is_binary(regid) do
    encode_requests(%{notification | registration_id: [regid]})
  end

  def encode_requests(%{registration_id: regid} = notification)
      when is_list(regid) do
    regid
    |> recipient_attr()
    |> Map.merge(notification.payload)
    |> Map.put("priority", to_string(notification.priority))
    |> Poison.encode!()
  end

  defp recipient_attr([regid]), do: %{"to" => regid}

  defp recipient_attr(regid) when is_list(regid),
    do: %{"registration_ids" => regid}
end
