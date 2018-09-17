defmodule Pigeon.ADM.Notification do
  @moduledoc """
  Defines Amazon ADM notification struct and convenience constructor functions.
  """

  defstruct consolidation_key: nil,
            expires_after: 604_800,
            md5: nil,
            payload: %{},
            registration_id: nil,
            response: nil,
            updated_registration_id: nil

  @typedoc ~S"""
  ADM notification

  ## Examples

      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        expires_after: 604_800,
        md5: "qzF+HgArKZjJrpfcTbiFxg==",
        payload: %{
          "data" => %{"message" => "your message"}
        },
        registration_id: "reg ID",
        response: nil, # Set on push response
        updated_registration_id: nil
      }
  """
  @type t :: %__MODULE__{
          consolidation_key: String.t(),
          expires_after: integer,
          md5: binary,
          payload: %{},
          registration_id: String.t(),
          response: response,
          updated_registration_id: String.t()
        }

  @typedoc ~S"""
  ADM push response

  - nil - Push has not been sent yet
  - `:success` - Push was successfully sent
  - `t:error_response/0` - Push attempted but server responded
    with error
  - `:timeout` - Internal error. Push did not reach ADM servers
  """
  @type response :: nil | :success | error_response | :timeout

  @typedoc ~S"""
  ADM error responses
  """
  @type error_response ::
          :access_token_expired
          | :invalid_registration_id
          | :invalid_data
          | :invalid_consolidation_key
          | :invalid_expiration
          | :invalid_checksum
          | :invalid_type
          | :max_rate_exceeded
          | :message_too_large
          | :unregistered
          | :unknown_error

  @doc ~S"""
  Creates `ADM.Notification` struct with device registration ID and optional data payload.

  ## Examples

      iex> Pigeon.ADM.Notification.new("reg ID")
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "1B2M2Y8AsgTpgAmY7PhCfg==",
        payload: %{"data" => %{}},
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.ADM.Notification.new("reg ID", %{"message" => "your message"})
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "qzF+HgArKZjJrpfcTbiFxg==",
        payload: %{
          "data" => %{"message" => "your message"}
        },
        registration_id: "reg ID",
        updated_registration_id: nil
      }

      iex> Pigeon.ADM.Notification.new("reg ID", "not a map")
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "1B2M2Y8AsgTpgAmY7PhCfg==",
        payload: %{"data" => %{}},
        registration_id: "reg ID",
        updated_registration_id: nil
      }
  """
  @spec new(String.t(), %{required(String.t()) => term}) :: t
  def new(registration_id, data \\ %{}) do
    %Pigeon.ADM.Notification{registration_id: registration_id}
    |> put_data(data)
  end

  @doc """
  Updates `"data"` key on push payload and calculates `md5` hash.

  ## Examples

      iex> n = %Pigeon.ADM.Notification{}
      iex> Pigeon.ADM.Notification.put_data(n, %{"message" => "your message"})
      %Pigeon.ADM.Notification{
        consolidation_key: nil,
        md5: "qzF+HgArKZjJrpfcTbiFxg==",
        payload: %{
          "data" => %{"message" => "your message"}
        },
        registration_id: nil,
        updated_registration_id: nil
      }
  """
  def put_data(n, data) do
    n
    |> update_payload("data", ensure_strings(data))
    |> calculate_md5
  end

  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)

    %{notification | payload: payload}
  end

  @doc false
  def ensure_strings(%{} = data) do
    data
    |> Enum.map(fn {key, value} -> {"#{key}", "#{value}"} end)
    |> Enum.into(%{})
  end

  def ensure_strings(_else), do: %{}

  @doc false
  def calculate_md5(%{payload: %{"data" => data}} = notification)
      when is_map(data) do
    concat =
      data
      |> Map.keys()
      |> Enum.sort()
      |> Enum.map(fn key -> "#{key}:#{data[key]}" end)
      |> Enum.join(",")

    md5 = :md5 |> :crypto.hash(concat) |> Base.encode64()

    %{notification | md5: md5}
  end

  def calculate_md5(notification), do: notification
end
