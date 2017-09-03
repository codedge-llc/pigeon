defmodule Pigeon do
  use Application
  require Logger
  import Supervisor.Spec

  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    Pigeon.Http2.Client.default.start
    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(workers(), opts)
  end

  defp workers do
    adm_workers() ++ apns_workers() ++ fcm_workers() ++ task_supervisors()
  end

  def task_supervisors do
    [supervisor(Task.Supervisor, [[name: Pigeon.Tasks]])]
  end

  def adm_workers do
    case Application.get_env(:pigeon, :adm) do
      nil -> []
      workers ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.ADM.Config.config(worker_name)
          worker(Pigeon.ADM.Worker, [config], id: worker_name, restart: :temporary)
        end)
    end
  end

  defp apns_workers do
    case Application.get_env(:pigeon, :apns) do
      nil -> []
      workers ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.APNS.Config.config(worker_name)
          worker(Pigeon.Worker, [config], id: worker_name, restart: :temporary)
        end)
    end
  end

  defp fcm_workers do
    case Application.get_env(:pigeon, :fcm) do
      nil -> []
      workers ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.FCM.Config.config(worker_name)
          worker(Pigeon.Worker, [config], id: worker_name, restart: :temporary)
        end)
    end
  end

  def debug_log?, do: Application.get_env(:pigeon, :debug_log, false)
end
