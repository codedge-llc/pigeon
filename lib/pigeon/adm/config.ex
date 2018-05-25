defmodule Pigeon.ADM.Config do
  @moduledoc """
  Validates configuration settings that initialize ADM.Worker instances.
  """

  defstruct name: nil, client_id: nil, client_secret: nil

  @type t :: %__MODULE__{
          client_id: String.t() | nil,
          client_secret: String.t() | nil,
          name: atom | nil
        }

  def default_name, do: :adm_default

  @doc ~S"""
  Returns a new `ADM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.ADM.Config.new(
      ...>   name: :test,
      ...>   client_id: "amzn.client.id",
      ...>   client_secret: "1234secret"
      ...> )
      %Pigeon.ADM.Config{name: :test, client_id: "amzn.client.id",
      client_secret: "1234secret"}
  """
  @spec new(Keyword.t() | atom) :: t
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      client_id: opts[:client_id],
      client_secret: opts[:client_secret]
    }
  end

  def new(name) when is_atom(name) do
    Application.get_env(:pigeon, :adm)[name]
    |> Enum.to_list()
    |> Keyword.put(:name, name)
    |> new()
  end

  @doc ~S"""
  Returns whether a given config has valid credentials.

  ## Examples

      iex> :adm_default |> new() |> valid?()
      true
  """
  def valid?(config) do
    valid_item?(config.client_id) and valid_item?(config.client_secret)
  end

  defp valid_item?(item), do: is_binary(item) and String.length(item) > 0
end
