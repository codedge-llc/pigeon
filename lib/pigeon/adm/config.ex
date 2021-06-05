defmodule Pigeon.ADM.Config do
  @moduledoc false

  defstruct client_id: nil, client_secret: nil

  @type t :: %__MODULE__{
          client_id: String.t() | nil,
          client_secret: String.t() | nil
        }

  @doc ~S"""
  Returns a new `ADM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.ADM.Config.new(
      ...>   client_id: "amzn.client.id",
      ...>   client_secret: "1234secret"
      ...> )
      %Pigeon.ADM.Config{
        client_id: "amzn.client.id",
        client_secret: "1234secret"
      }
  """
  @spec new(Keyword.t() | atom) :: t
  def new(opts) when is_list(opts) do
    %__MODULE__{
      client_id: opts[:client_id],
      client_secret: opts[:client_secret]
    }
  end

  @doc ~S"""
  Returns whether a given config has valid credentials.

  ## Examples

      iex> [] |> new() |> valid?()
      false
  """
  def valid?(config) do
    valid_item?(config.client_id) and valid_item?(config.client_secret)
  end

  defp valid_item?(item), do: is_binary(item) and String.length(item) > 0

  @spec validate!(any) :: :ok | no_return
  def validate!(config) do
    if valid?(config) do
      :ok
    else
      raise Pigeon.ConfigError,
        reason: "attempted to start without valid client id and secret",
        config: redact(config)
    end
  end

  defp redact(config) do
    [:client_id, :client_secret]
    |> Enum.reduce(config, fn k, acc ->
      case Map.get(acc, k) do
        nil -> acc
        _ -> Map.put(acc, k, "[FILTERED]")
      end
    end)
  end
end
