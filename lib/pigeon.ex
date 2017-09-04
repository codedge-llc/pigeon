defmodule Pigeon do
  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  use Application

  require Logger
  import Supervisor.Spec

  alias Pigeon.{ADM, APNS, FCM}

  @doc false
  def start(_type, _args) do
    Pigeon.Http2.Client.default().start
    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(workers(), opts)
  end

  defp workers do
    adm_workers()
    ++ apns_workers()
    ++ fcm_workers()
    ++ env_workers()
    ++ task_supervisors()
  end

  defp task_supervisors do
    [supervisor(Task.Supervisor, [[name: Pigeon.Tasks]])]
  end

  defp env_workers do
    case Application.get_env(:pigeon, :workers) do
      nil -> []
      workers ->
        Enum.map(workers, fn({mod, fun}) ->
          config = apply(mod, fun, [])
          worker(Pigeon.Worker, [config], id: config.name, restart: :temporary)
        end)
    end
  end

  defp adm_workers do
    workers_for(:adm, &ADM.Config.new/1, Pigeon.ADM.Worker)
  end

  defp apns_workers do
    workers_for(:apns, &APNS.Config.new/1, Pigeon.Worker)
  end

  defp fcm_workers do
    workers_for(:fcm, &FCM.Config.new/1, Pigeon.Worker)
  end

  defp workers_for(name, config_fn, mod) do
    case Application.get_env(:pigeon, name) do
      nil -> []
      workers ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = config_fn.(worker_name)
          worker(mod, [config], id: config.name, restart: :temporary)
        end)
    end
  end

  def debug_log?, do: Application.get_env(:pigeon, :debug_log, false)
end
