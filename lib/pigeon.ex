defmodule Pigeon do
  use Application
  require Logger

  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args), do: Pigeon.Supervisor.start_link

  def start_child(name, mode, cert, key) do
    config = %{
      mode: mode,
      cert: Pigeon.Supervisor.cert(cert),
      certfile: Pigeon.Supervisor.file_path(cert),
      key: Pigeon.Supervisor.key(key),
      keyfile: Pigeon.Supervisor.file_path(key)
    }
    poolboy_config = [
      {:name, {:local, name}},
      {:worker_module, Pigeon.APNSWorker},
      {:size, 5},
      {:max_overflow, 5}
    ]
    Supervisor.start_child(:pigeon, :poolboy.child_spec(name, poolboy_config, config))
  end
end
