defmodule Pigeon.APNS do
  @moduledoc """
    Defines publically-exposed Apple Push Notification Service (APNS) functions. For implementation
    see APNSWorker.
  """
  require Logger
  alias Pigeon.APNS.Config

  @doc """
    Sends a push over APNS.
  """
  def push(notification, opts \\ []) do
    pool_name = opts[:name] || :default
    case opts[:on_response] do
      nil -> Pigeon.Supervisor.push(pool_name, notification)
      on_response -> Pigeon.Supervisor.push(pool_name, notification, on_response)
    end
  end

  def start_default_connection do
    cond do
      !Config.default_keys? ->
        []
      Pigeon.APNS.Config.valid?(Config.default_config) ->
        poolboy_config = [
          {:name, {:local, default_pool}},
          {:worker_module, Pigeon.APNSWorker},
          {:size, 5},
          {:max_overflow, 5}
        ]
        spec = :poolboy.child_spec(default_pool, poolboy_config, Config.default_config)
        Supervisor.start_child(:pigeon, spec)
      true ->
        Logger.error "Error starting :apns_worker. Invalid mode/cert/key configuration."
        []
    end
  end

  defp default_pool, do: :default

  def start_connection(name, mode, cert, key) do
    config = %{
      mode: mode,
      cert: Config.cert(cert),
      certfile: Config.file_path(cert),
      key: Config.key(key),
      keyfile: Config.file_path(key)
    }
    poolboy_config = [
      {:name, {:local, name}},
      {:worker_module, Pigeon.APNSWorker},
      {:size, 5},
      {:max_overflow, 5}
    ]
    Supervisor.start_child(:pigeon, :poolboy.child_spec(name, poolboy_config, config))
  end

  def stop_connection(name) do
    Supervisor.terminate_child(:pigeon, name)
    Supervisor.delete_child(:pigeon, name)
  end
end
