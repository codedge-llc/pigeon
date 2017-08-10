defmodule Pigeon.FCM.Config do
  defstruct key: nil,
            name: nil,
            ping_period: 600_000,
            reconnect: false

  @type t :: %__MODULE__{
      key: binary,
      name: term,
      ping_period: pos_integer,
      reconnect: boolean
    }

  def new(key, opts) do
    %__MODULE__{
      key: key,
      name: opts[:name] || nil,
      ping_period: opts[:ping_period] || 600_000,
      reconnect: opts[:reconnect] || false
    }
  end
end
