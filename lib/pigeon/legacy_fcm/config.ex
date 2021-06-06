defmodule Pigeon.LegacyFCM.Config do
  @moduledoc false

  defstruct key: nil,
            uri: 'fcm.googleapis.com',
            port: 443

  @type t :: %__MODULE__{
          key: binary,
          port: pos_integer,
          uri: charlist
        }

  @doc ~S"""
  Returns a new `LegacyFCM.Config` with given `opts`.

  ## Examples

      iex> Pigeon.LegacyFCM.Config.new(
      ...>   key: "fcm_key",
      ...>   uri: 'test.server.example.com',
      ...>   port: 5228
      ...> )
      %Pigeon.LegacyFCM.Config{
        key: "fcm_key",
        port: 5228, 
        uri: 'test.server.example.com'
      }
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      key: Keyword.get(opts, :key),
      uri: Keyword.get(opts, :uri, 'fcm.googleapis.com'),
      port: Keyword.get(opts, :port, 443)
    }
  end
end

defimpl Pigeon.Configurable, for: Pigeon.LegacyFCM.Config do
  @moduledoc false

  require Logger

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Encodable
  alias Pigeon.LegacyFCM.{Config, ResultParser}

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

  def push_headers(%Config{key: key}, _notification, opts) do
    [
      {":method", "POST"},
      {":path", "/fcm/send"},
      {"authorization", "key=#{opts[:key] || key}"},
      {"content-type", "application/json"},
      {"accept", "application/json"}
    ]
  end

  def push_payload(_config, notification, _opts) do
    Encodable.binary_payload(notification)
  end

  def handle_end_stream(_config, %{error: nil} = stream, notif) do
    do_handle_end_stream(stream.status, stream.body, notif)
  end

  def handle_end_stream(_config, _stream, {_regids, notif}) do
    notif
    |> Map.put(:status, :unavailable)
    |> process_on_response()
  end

  defp do_handle_end_stream(200, body, notif) do
    result = Pigeon.json_library().decode!(body)
    notif = %{notif | status: :success}

    notif.registration_id
    |> parse_result(result, notif)
    |> process_on_response()
  end

  defp do_handle_end_stream(400, _body, notif) do
    notif
    |> Map.put(:status, :malformed_json)
    |> process_on_response()
  end

  defp do_handle_end_stream(401, _body, notif) do
    notif
    |> Map.put(:status, :unauthorized)
    |> process_on_response()
  end

  defp do_handle_end_stream(500, _body, notif) do
    notif
    |> Map.put(:status, :internal_server_error)
    |> process_on_response()
  end

  defp do_handle_end_stream(_code, body, notif) do
    reason = parse_error(body)

    notif
    |> Map.put(:response, reason)
    |> process_on_response()
  end

  def schedule_ping(_config), do: :ok

  def close(_config) do
  end

  def validate!(%{key: key}) when is_binary(key) do
    :ok
  end

  def validate!(config) do
    raise Pigeon.ConfigError,
      reason: "attempted to start without valid key",
      config: redact(config)
  end

  defp redact(%{key: key} = config) when is_binary(key) do
    Map.put(config, :key, "[FILTERED]")
  end

  defp redact(config), do: config

  def parse_result(ids, %{"results" => results}, notification) do
    ResultParser.parse(ids, results, notification)
  end

  def parse_result(id, %{"message_id" => _} = result, notification)
      when is_binary(id) do
    parse_result([id], %{"results" => [result]}, notification)
  end

  def parse_error(data) do
    case Pigeon.json_library().decode(data) do
      {:ok, response} ->
        response["reason"] |> Macro.underscore() |> String.to_existing_atom()

      error ->
        "JSON parse failed: #{inspect(error)}, body: #{inspect(data)}"
        |> Logger.error()
    end
  end
end
