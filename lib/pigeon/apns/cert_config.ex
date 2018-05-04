defmodule Pigeon.APNS.CertConfig do
  @moduledoc """
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
end

defimpl Pigeon.Configurable, for: Pigeon.APNS.CertConfig do
  @moduledoc false

  alias Pigeon.APNS.Shared

  # Configurable Callbacks

  defdelegate worker_name(any), to: Shared

  defdelegate max_demand(any), to: Shared

  defdelegate connect(any), to: Shared

  defdelegate push_headers(config, notification, opts), to: Shared

  defdelegate push_payload(config, notification, opts), to: Shared

  defdelegate handle_end_stream(config, stream, notification, on_response),
    to: Shared

  defdelegate schedule_ping(any), to: Shared

  defdelegate close(config), to: Shared
end
