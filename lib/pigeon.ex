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
    adm_workers() ++ apns_workers() ++ fcm_workers() ++ task_supervisors()
  end

  def task_supervisors do
    [supervisor(Task.Supervisor, [[name: Pigeon.Tasks]])]
  end

  def adm_workers do
    cond do
      workers = Application.get_env(:pigeon, :adm) ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.ADM.Config.config(worker_name)
          worker(Pigeon.ADM.Worker, [config], id: worker_name)
        end)
      true -> []
    end
  end

  defp apns_workers do
    cond do
      workers = Application.get_env(:pigeon, :apns) ->
        Enum.map(workers, fn({worker_name, _config}) ->
          config = Pigeon.APNS.Config.config(worker_name)
          worker(Pigeon.APNS.Worker, [config], id: worker_name)
        end)
      true -> []
    end
  end

  defp fcm_workers do
    # cond do
    #   config = Application.get_env(:pigeon, :fcm) ->
    #     [worker(Pigeon.FCMWorker, [:fcm_worker, config], id: :fcm_worker)]
    #   true -> []
    # end
    cond do
      workers = Application.get_env(:pigeon, :fcm) ->
        Enum.map(workers, fn({worker_name, config}) ->
          config = Map.put(config, :name, worker_name)
          worker(Pigeon.FCM.Worker, [config], id: worker_name)
        end)
      true -> []
    end
  end
end
