defmodule Pigeon.APNS.Config do
  @moduledoc ~S"""
  Configuration for APNS Workers
  """

  defstruct name: nil,
            reconnect: true,
            cert: nil,
            certfile: nil,
            key: nil,
            keyfile: nil,
            uri: nil,
            port: 443,
            ping_period: 600_000

  @typedoc ~S"""
  APNS configuration struct

  This struct should not be set directly. Instead use `new/1`
  with `t:config_opts/0`.

  ## Examples

      %Pigeon.APNS.Config{
        name: :apns_default,
        reconnect: true,
        cert: nil,
        certfile: "cert.pem",
        key: nil,
        keyfile: "key.pem",
        uri: "api.push.apple.com",
        port: 443,
        ping_period: 600_000
      }
  """
  @type t :: %__MODULE__{
          name: atom | nil,
          reconnect: boolean,
          cert: binary | nil,
          certfile: binary | nil,
          key: binary | nil,
          keyfile: binary | nil,
          uri: binary | nil,
          port: pos_integer,
          ping_period: pos_integer
        }

  alias Pigeon.APNS.ConfigParser

  @doc false
  def default_name, do: :apns_default

  @doc ~S"""
  Returns a new `APNS.Config` or `APNS.JWTConfig` with given `opts` or name.

  If given an atom, returns the config specified in your `mix.exs`.

  ## Examples

      iex> Pigeon.APNS.Config.new(
      ...>   name: :test,
      ...>   mode: :prod,
      ...>   cert: "test_cert.pem",
      ...>   key: "test_key.pem",
      ...>   port: 2197,
      ...>   ping_period: 300_000
      ...> )
      %Pigeon.APNS.Config{uri: "api.push.apple.com", name: :test,
      ping_period: 300000, port: 2197, reconnect: false}

      iex> config = Pigeon.APNS.Config.new(:apns_default)
      iex> %{config | certfile: nil, keyfile: nil} # Hide for testing
      iex> match? %_{uri: "api.development.push.apple.com",
      ...> name: :apns_default, ping_period: 600_000, port: 443}, config
      true
  """
  def new(opts) when is_list(opts) do
    %__MODULE__{
      name: opts[:name],
      reconnect: Keyword.get(opts, :reconnect, false),
      cert: ConfigParser.cert(opts[:cert]),
      certfile: ConfigParser.file_path(opts[:cert]),
      key: ConfigParser.key(opts[:key]),
      keyfile: ConfigParser.file_path(opts[:key]),
      uri: Keyword.get(opts, :uri, ConfigParser.uri_for_mode(opts[:mode])),
      port: Keyword.get(opts, :port, 443),
      ping_period: Keyword.get(opts, :ping_period, 600_000)
    }
  end

  def new(name) when is_atom(name), do: ConfigParser.parse(name)
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  alias Pigeon.APNS.Shared

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  defdelegate worker_name(any), to: Shared

  defdelegate max_demand(any), to: Shared

  @spec connect(any) :: {:ok, sock} | {:error, String.t()}
  def connect(%{uri: uri} = config) do
    uri = to_charlist(uri)

    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)

      error ->
        error
    end
  end

  defdelegate push_headers(config, notification, opts), to: Shared

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared

  def connect_socket_options(%{cert: nil, certfile: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(%{key: nil, keyfile: nil}) do
    {:error, :invalid_config}
  end

  def connect_socket_options(config) do
    options =
      [
        cert_option(config),
        key_option(config),
        {:password, ''},
        {:packet, 0},
        {:reuseaddr, true},
        {:active, true},
        :binary
      ]
      |> Shared.add_port(config)

    {:ok, options}
  end

  def cert_option(%{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%{key: nil, keyfile: file}), do: {:keyfile, file}
end
