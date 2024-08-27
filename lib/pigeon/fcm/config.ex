defmodule Pigeon.FCM.Config do
  @moduledoc false

  defstruct port: 443,
            project_id: nil,
            service_account_json: nil,
            uri: ~c"fcm.googleapis.com"

  @type t :: %__MODULE__{
          port: pos_integer,
          project_id: binary,
          service_account_json: binary,
          uri: charlist
        }

  @doc ~S"""
  Returns a new `FCM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.FCM.Config.new(
      ...>   project_id: "example-project",
      ...>   service_account_json: "{\"dummy\":\"contents\"}"
      ...> )
      %Pigeon.FCM.Config{
        port: 443,
        project_id: "example-project",
        service_account_json: %{"dummy" => "contents"},
        uri: 'fcm.googleapis.com'
      }
  """
  def new(opts) when is_list(opts) do
    project_id =
      opts
      |> Keyword.get(:project_id)
      |> decode_bin()

    service_account_json =
      opts
      |> Keyword.get(:service_account_json)
      |> decode_json()

    %__MODULE__{
      port: Keyword.get(opts, :port, 443),
      project_id: project_id,
      service_account_json: service_account_json,
      uri: Keyword.get(opts, :uri, ~c"fcm.googleapis.com")
    }
  end

  def decode_bin(bin) when is_binary(bin) do
    bin
  end

  def decode_bin(other) do
    {:error, {:invalid, other}}
  end

  def decode_json(bin) when is_binary(bin) do
    case Pigeon.json_library().decode(bin) do
      {:ok, json} -> json
      {:error, _reason} -> {:error, {:invalid, bin}}
    end
  end

  def decode_json(other) do
    {:error, {:invalid, other}}
  end
end

defimpl Pigeon.Configurable, for: Pigeon.FCM.Config do
  @moduledoc false

  require Logger

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Encodable
  alias Pigeon.FCM.{Config, Error}

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
        {:active, :once},
        {:packet, :raw},
        {:reuseaddr, true},
        {:alpn_advertised_protocols, [<<"h2">>]},
        :binary
      ]
      |> add_port(config)

    {:ok, opts}
  end

  def add_port(opts, %Config{port: 443}), do: opts
  def add_port(opts, %Config{port: port}), do: [{:port, port} | opts]

  def push_headers(
        %Config{project_id: project_id},
        _notification,
        opts
      ) do
    [
      {":method", "POST"},
      {":path", "/v1/projects/#{project_id}/messages:send"},
      {"authorization", "Bearer #{opts[:token].token}"},
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

  def validate!(%{project_id: {:error, _}} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :project_id",
      config: redact(config)
  end

  def validate!(%{service_account_json: {:error, _}} = config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid :service_account_json",
      config: redact(config)
  end

  def validate!(_config), do: :ok

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
