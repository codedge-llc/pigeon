defmodule Pigeon.FCM.Config do
  @moduledoc false

  defstruct token_fetcher: nil,
            project_id: nil,
            uri: "fcm.googleapis.com",
            port: 443

  @typedoc """
  the name, or custom module, of your Goth implementation, e.g. `YourApp.Goth`.

  This is passed directly to `Goth.fetch!/1`.
  """
  @type token_fetcher :: module() | term()

  @type t :: %__MODULE__{
          token_fetcher: nil | token_fetcher(),
          project_id: nil | String.t(),
          uri: String.t(),
          port: pos_integer()
        }

  @doc ~S"""
  Returns a new `FCM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.FCM.Config.new(
      ...>   project_id: "example-project",
      ...>   token_fetcher: YourApp.Goth
      ...> )
      %Pigeon.FCM.Config{
        port: 443,
        project_id: "example-project",
        token_fetcher: YourApp.Goth,
        uri: "fcm.googleapis.com"
      }
  """
  def new(opts) do
    opts = Map.new(opts)

    %__MODULE__{
      token_fetcher: opts[:token_fetcher],
      project_id: opts[:project_id],
      uri: Map.get(opts, :uri, "fcm.googleapis.com"),
      port: Map.get(opts, :port, 443)
    }
  end
end

defimpl Pigeon.Configurable, for: Pigeon.FCM.Config do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Encodable
  alias Pigeon.FCM.Error

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(%@for{uri: uri} = config) do
    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)
    end
  end

  def connect_socket_options(config) do
    opts =
      [
        {:active, :once},
        {:packet, :raw},
        {:reuseaddr, true},
        {:alpn_advertised_protocols, [<<"h2">>]},
        :binary
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
    token = Goth.fetch!(config.token_fetcher)

    [
      {":method", "POST"},
      {":path", "/v1/projects/#{config.project_id}/messages:send"},
      {"authorization", "#{token.type} #{token.token}"},
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

  def validate!(config) do
    config
    |> Map.from_struct()
    |> Enum.each(&do_validate!(&1, config))
  end

  defp do_validate!({:token_fetcher, mod}, config)
       when not is_atom(mod) or is_nil(mod) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :token_fetcher module",
      config: redact(config)
  end

  defp do_validate!({:project_id, value}, config) when not is_binary(value) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :project_id",
      config: redact(config)
  end

  defp do_validate!({_key, _value}, _config), do: :ok

  @doc false
  def redact(config) when is_map(config) do
    [:service_account_json]
    |> Enum.reduce(config, fn key, acc ->
      case Map.get(acc, key) do
        val when is_map(val) -> Map.put(acc, key, "[FILTERED]")
        _ -> acc
      end
    end)
  end
end
