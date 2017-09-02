defmodule Pigeon.APNS.Config do
  @moduledoc """
  Validates configuration settings that initialize APNS.Workers.
  """

  defstruct name: nil,
            mode: nil,
            reconnect: true,
            cert: nil,
            certfile: nil,
            key: nil,
            keyfile: nil,
            port: 443,
            ping_period: 600_000

  @type t :: %__MODULE__{
    name: atom | nil,
    mode: :dev | :prod,
    reconnect: boolean,
    cert: binary | nil,
    certfile: binary | nil,
    key: binary | nil,
    keyfile: binary | nil,
    port: pos_integer,
    ping_period: pos_integer
  }

  def default_name, do: :apns_default

  def config(name) do
    config = Application.get_env(:pigeon, :apns)[name]
    %__MODULE__{
      name: name,
      mode: mode(config[:mode]),
      reconnect: Map.get(config, :reconnect, true),
      cert: cert(config[:cert]),
      certfile: file_path(config[:cert]),
      key: key(config[:key]),
      keyfile: file_path(config[:key]),
      port: config[:port] || 443,
      ping_period: config[:ping_period] || 600_000
    }
  end

  def mode({:system, env_var}), do: to_mode(System.get_env(env_var))
  def mode(mode), do: mode

  def to_mode(nil), do: raise "APNS.Config mode is nil"
  def to_mode("dev"), do: :dev
  def to_mode(":dev"), do: :dev
  def to_mode("prod"), do: :prod
  def to_mode(":prod"), do: :prod
  def to_mode(other), do: raise "APNS.Config mode is #{inspect(other)}"

  def file_path(nil), do: nil
  def file_path(path) when is_binary(path) do
    cond do
      :filelib.is_file(path) -> Path.expand(path)
      true -> nil
    end
  end
  def file_path({app_name, path}) when is_atom(app_name),
    do: Path.expand(path, :code.priv_dir(app_name))

  def cert({_app_name, _path}), do: nil
  def cert(nil), do: nil
  def cert(bin) do
    case :public_key.pem_decode(bin) do
      [{:Certificate, cert, _}] -> cert
      _ -> nil
    end
  end

  def key({_app_name, _path}), do: nil
  def key(nil), do: nil
  def key(bin) do
    case :public_key.pem_decode(bin) do
      [{:RSAPrivateKey, key, _}] -> {:RSAPrivateKey, key}
      _ -> nil
    end
  end

  def valid?(config) do
    valid_mode? = (config[:mode] == :dev || config[:mode] == :prod)
    valid_cert? = !is_nil(config[:cert] || config[:certfile])
    valid_key? = !is_nil(config[:key] || config[:keyfile])
    valid_mode? && valid_cert? && valid_key?
  end
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.Config do
  @moduledoc false

  alias Pigeon.APNS.{Config, Error}

  @type sock :: {:sslsocket, any, pid | {any, any}}

  # Configurable Callbacks

  @spec worker_name(any) :: atom | nil
  def worker_name(%Config{name: name}), do: name

  @spec connect(any) :: {:ok, sock} | {:error, String.t}
  def connect(%Config{mode: mode} = config) do
    uri = mode |> push_uri |> to_charlist
    case connect_socket_options(config) do
      {:ok, options} ->
        Pigeon.Http2.Client.default().connect(uri, :https, options)
      error -> error
    end
  end

  def push_headers(_config, notification, _opts) do
    json = Pigeon.Notification.json_payload(notification.payload)

    [
      {":method", "POST"},
      {":path", "/3/device/#{notification.device_token}"},
      {"content-length", "#{byte_size(json)}"}
    ]
    |> put_apns_id(notification)
    |> put_apns_topic(notification)
  end

  def push_payload(_config, notification, _opts) do
    Pigeon.Notification.json_payload(notification.payload)
  end

  defp put_apns_id(headers, notification) do
    case notification.id do
      nil -> headers
      id -> headers ++ [{"apns-id", id}]
    end
  end

  defp put_apns_topic(headers, notification) do
    case notification.topic do
      nil   -> headers
      topic -> headers ++ [{"apns-topic", topic}]
    end
  end

  def handle_end_stream(_config,
                        %{headers: headers, body: body, status: status},
                        notification,
                        on_response) do
    case status do
      200 ->
        notification = %{notification | id: get_apns_id(headers)}
        unless on_response == nil, do: on_response.({:ok, notification})
      _error ->
        reason = Error.parse(body)
        Error.log(reason, notification)
        unless on_response == nil, do: on_response.({:error, reason, notification})
    end
  end

  def get_apns_id(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == "apns-id" end) do
      {"apns-id", id} -> id
      nil -> nil
    end
  end

  @doc ~S"""
  Returns ping period for config.

  ## Examples

      iex> config = %Pigeon.APNS.Config{ping_period: 60}
      iex> Pigeon.Configurable.ping_period(config)
      60
  """
  @spec ping_period(any) :: pos_integer
  def ping_period(%Config{ping_period: ping}), do: ping

  @spec reconnect?(any) :: boolean
  def reconnect?(%Config{reconnect: reconnect}), do: reconnect

  def close(_config) do
  end

  # Everything Else

  def connect_socket_options(%Config{cert: nil, certfile: nil}) do
    {:error, :invalid_config}
  end
  def connect_socket_options(%Config{key: nil, keyfile: nil}) do
    {:error, :invalid_config}
  end
  def connect_socket_options(config) do
    options = [
      cert_option(config),
      key_option(config),
      {:password, ''},
      {:packet, 0},
      {:reuseaddr, true},
      {:active, true},
      :binary
    ]
    |> add_port(config)

    {:ok, options}
  end

  def cert_option(%Config{cert: cert, certfile: nil}), do: {:cert, cert}
  def cert_option(%Config{cert: nil, certfile: file}), do: {:certfile, file}

  def key_option(%Config{key: key, keyfile: nil}), do: {:key, key}
  def key_option(%Config{key: nil, keyfile: file}), do: {:keyfile, file}

  defp add_port(opts, %Config{port: 443}), do: opts
  defp add_port(opts, %Config{port: port}), do: [{:port, port} | opts]

  defp apns_production_api_uri, do: "api.push.apple.com"
  defp apns_development_api_uri, do: "api.development.push.apple.com"

  defp push_uri(mode) do
    case mode do
      :dev -> apns_development_api_uri()
      :prod -> apns_production_api_uri()
    end
  end
end
