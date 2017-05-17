defmodule Pigeon do
  use Application
  require Logger
  import Supervisor.Spec

  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.
  """

  def start(_type, _args) do
    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(workers(), opts)
  end

  defp workers do
    adm_worker() ++ apns_workers() ++ ++ gcm_workers() ++ task_supervisors()
  end

  def task_supervisors do
    [supervisor(Task.Supervisor, [[name: Pigeon.Tasks]])]
  end

  def adm_worker do
    cond do
      !Pigeon.ADM.Config.configured? ->
        []
      Pigeon.ADM.Config.valid?(config = Pigeon.ADM.Config.default_config) ->
        [worker(Pigeon.ADMWorker, [:adm_worker, config], id: :adm_worker)]
      true ->
        Logger.error "Error starting :adm_worker. Invalid OAuth2 configuration."
        []
    end
  end

  defp apns_workers do
    cond do
      workers = Application.get_env(:pigeon, :apns) ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.APNS.Config.config(worker_name)
          worker(Pigeon.APNSWorker, [config], id: worker_name)
        end)
      true -> []
    end
  end

  defp gcm_workers do
    cond do
      config = Application.get_env(:pigeon, :gcm) ->
        [worker(Pigeon.GCMWorker, [:gcm_worker, config], id: :gcm_worker)]
      true -> []
    end
  end
end
