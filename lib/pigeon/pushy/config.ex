defmodule Pigeon.Pushy.Config do
  @moduledoc false

  defstruct key: nil,
            port: 443,
            uri: nil

  @typedoc ~S"""
  Pushy configuration struct

  This struct should not be set directly. Instead, use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.Pushy.Config{
        key: "some-secret-key",
        uri: "api.pushy.me",
        port: 443
      }
  """
  @type t :: %__MODULE__{
          key: binary | nil,
          uri: binary | nil,
          port: pos_integer
        }

  @typedoc ~S"""
  Options for configuring Pushy connections.

  ## Configuration Options
  - `:key` - Pushy secrety key.
  - `:uri` - Pushy server uri.
  - `:port` - Push server port. Can be any value, but Pushy only accepts
    `443`
  """
  @type config_opts :: [
          key: binary,
          uri: binary,
          port: pos_integer
        ]

  @doc false
  def default_name, do: :pushy_default

  @doc ~S"""
  Returns a new `Pushy.Config` with given `opts`.

  ## Examples

      iex> Pigeon.Pushy.Config.new(
      ...>   key: System.get_env("PUSHY_SECRET_KEY"),
      ...>   uri: "api.pushy.me",
      ...>   port: 443
      ...> )
      %Pigeon.Pushy.Config{
        key: System.get_env("PUSHY_SECRET_KEY")
        port: 443,
        uri: "api.pushy.me"
      }
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      key: opts |> Keyword.get(:key),
      uri: Keyword.get(opts, :uri, 'api.pushy.me'),
      port: Keyword.get(opts, :port, 443)
    }
  end
end
