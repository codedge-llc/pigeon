defmodule Pigeon.FCM.Config do
  @moduledoc false

  defstruct auth: nil,
            project_id: nil,
            uri: ~c"fcm.googleapis.com",
            port: 443

  @typedoc """
  The name, or custom module, of your Goth implementation, e.g. `YourApp.Goth`.

  This is passed directly to `Goth.fetch!/1`.
  """
  @type auth :: module() | term()

  @type t :: %__MODULE__{
          auth: nil | auth(),
          project_id: nil | String.t(),
          uri: String.t(),
          port: pos_integer()
        }

  @doc ~S"""
  Returns a new `FCM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.FCM.Config.new(
      ...>   auth: YourApp.Goth,
      ...>   project_id: "example-project"
      ...> )
      %Pigeon.FCM.Config{
        auth: YourApp.Goth,
        port: 443,
        project_id: "example-project",
        uri: ~c"fcm.googleapis.com"
      }
  """
  def new(opts) do
    opts = Map.new(opts)

    %__MODULE__{
      auth: opts[:auth],
      port: Map.get(opts, :port, 443),
      project_id: opts[:project_id],
      uri: Map.get(opts, :uri, ~c"fcm.googleapis.com")
    }
  end
end

defimpl Pigeon.Configurable, for: Pigeon.FCM.Config do
  @moduledoc false

  alias Pigeon.Encodable

  # Configurable Callbacks

  @spec connect(any) :: {:ok, Mint.HTTP2.t()} | {:error, Exception.t()}
  def connect(%@for{uri: uri, port: port} = config) do
    {:ok, options} = connect_socket_options(config)

    client_settings = [
      initial_window_size: round(:math.pow(2, 31) - 1),
      max_frame_size: round(:math.pow(2, 24) - 1)
    ]

    Mint.HTTP2.connect(:https, to_string(uri), port,
      transport_opts: options,
      client_settings: client_settings
    )
  end

  def connect_socket_options(config) do
    opts =
      [
        {:active, :once},
        {:packet, :raw},
        {:reuseaddr, true},
        {:alpn_advertised_protocols, [<<"h2">>]}
      ]
      |> add_port(config)

    {:ok, opts}
  end

  def add_port(opts, %@for{port: 443}), do: opts
  def add_port(opts, %@for{port: port}), do: [{:port, port} | opts]

  def push_headers(
        config,
        _notification,
        _opts
      ) do
    token = Goth.fetch!(config.auth)

    [
      {"accept", "application/json"},
      {"authorization", "#{token.type} #{token.token}"},
      {"content-type", "application/json"}
    ]
  end

  def push_payload(_config, notification, _opts) do
    Encodable.binary_payload(notification)
  end

  def schedule_ping(_config), do: :ok

  def close(_config) do
  end

  def validate!(config) do
    config
    |> Map.from_struct()
    |> Enum.each(&do_validate!(&1, config))
  end

  defp do_validate!({:auth, mod}, config)
       when not is_atom(mod) or is_nil(mod) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :auth module",
      config: config
  end

  defp do_validate!({:project_id, value}, config) when not is_binary(value) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :project_id",
      config: config
  end

  defp do_validate!({_key, _value}, _config), do: :ok
end
