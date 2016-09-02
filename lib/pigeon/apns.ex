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
    pool_name = opts[:name] || Config.default_pool
    case opts[:on_response] do
      nil -> Pigeon.Supervisor.push(pool_name, notification)
      on_response -> Pigeon.Supervisor.push(pool_name, notification, on_response)
    end
  end

  def start_connection(name) do
    config = Config.config(name)
    poolboy_config = [
      {:name, {:local, name}},
      {:worker_module, Pigeon.APNSWorker},
      {:size, config[:pool_size]},
      {:max_overflow, config[:max_overflow]}
    ]
    Supervisor.start_child(:pigeon, :poolboy.child_spec(name, poolboy_config, config))
  end

  def start_connection(name, opts) do
    config = %{
      mode: opts[:mode],
      cert: Config.cert(opts[:cert]),
      certfile: Config.file_path(opts[:cert]),
      key: Config.key(opts[:key]),
      keyfile: Config.file_path(opts[:key])
    }
    poolboy_config = [
      {:name, {:local, name}},
      {:worker_module, Pigeon.APNSWorker},
      {:size, opts[:pool_size] || Config.default_pool_size},
      {:max_overflow, opts[:max_overflow] || Config.default_max_overflow}
    ]
    Supervisor.start_child(:pigeon, :poolboy.child_spec(name, poolboy_config, config))
  end

  def stop_connection(name) do
    Supervisor.terminate_child(:pigeon, name)
    Supervisor.delete_child(:pigeon, name)
  end
end
