defmodule Pigeon.ADM.Notification do
  @moduledoc """
  Defines Amazon ADM notification struct and convenience constructor functions.
  """
  defstruct registration_id: nil, payload: %{}, updated_registration_id: nil,
            consolidation_key: nil, expires_after: 604800, md5: nil

  @type t :: %__MODULE__{
    consolidation_key: String.t,
    expires_after: integer,
    md5: binary,
    payload: %{},
    registration_id: String.t,
    updated_registration_id: String.t
  }

  @doc """
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
  """
  def new(registration_id, data \\ %{})
  def new(registration_id, data) do
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

  defp update_payload(notification, key, value) when value == %{} and key != "data",
    do: notification
  defp update_payload(notification, key, value) do
    payload =
      notification.payload
      |> Map.put(key, value)
    %{notification | payload: payload}
  end

  @doc """
  ADM requires that "data" keys and values are all strings.
  """
  def ensure_strings(data) do
    data
    |> Enum.map(fn {key, value} -> {"#{key}", "#{value}"} end)
    |> Enum.into(%{})
  end

  @doc """
  Calculates md5 hash of notification data payload.
  """
  def calculate_md5(%{payload: %{"data" => data}} = notification) when is_map(data) do
    concat =
      data
      |> Map.keys
      |> Enum.sort
      |> Enum.map(fn key -> "#{key}:#{data[key]}" end)
      |> Enum.join(",")

    md5 = :md5 |> :crypto.hash(concat) |> Base.encode64

    %{notification | md5: md5}
  end
  def calculate_md5(notification), do: notification
end
