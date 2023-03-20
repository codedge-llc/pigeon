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

defimpl Pigeon.Configurable, for: Pigeon.Pushy.Config do
  @moduledoc false

  require Logger

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Encodable
  alias Pigeon.Pushy.{Config, Error}

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(%Config{uri: uri} = config) do
    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)
    end
  end

  def connect_socket_options(config) do
    opts =
      [
        {:active, true},
        {:packet, :raw},
        {:reuseaddr, true},
        :binary
      ]
      |> add_port(config)

    {:ok, opts}
  end

  def add_port(opts, %Config{port: 443}), do: opts
  def add_port(opts, %Config{port: port}), do: [{:port, port} | opts]

  def push_headers(
        %Config{key: key},
        _notification,
        _opts
      ) do
    [
      {":method", "POST"},
      {":path", "/push/?api_key=#{key}"},
      {"content-type", "application/json"},
      {"accept", "application/json"}
    ]
  end

  def push_payload(_config, notification, _opts) do
    Encodable.binary_payload(notification)
  end

  def handle_end_stream(_config, %{error: nil} = stream, notif) do
    stream.body
    |> Pigeon.json_library().decode!()
    |> case do
      %{"name" => name} ->
        notif
        |> Map.put(:name, name)
        |> Map.put(:response, :success)
        |> process_on_response()

      %{"error" => error} ->
        notif
        |> Map.put(:error, error)
        |> Map.put(:response, Error.parse(error))
        |> process_on_response()
    end
  end

  def schedule_ping(_config), do: :ok

  def close(_config) do
  end

  def validate!(_config), do: :ok

  @doc false
  def redact(config) when is_map(config) do
    [:key]
    |> Enum.reduce(config, fn key, acc ->
      case Map.get(acc, key) do
        val when is_map(val) -> Map.put(acc, key, "[FILTERED]")
        _ -> acc
      end
    end)
  end
end
