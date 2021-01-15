defmodule Pigeon.Application do
  @moduledoc false

  use Application
  alias Pigeon.{APNS, FCM}
  alias Pigeon.Http2.Client

  @doc false
  def start(_type, _args) do
    Client.default().start
    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(workers(), opts)
  end

  defp workers do
    [
      PigeonTest.ADM,
      PigeonTest.APNS,
      PigeonTest.FCM,
      apns_workers(),
      fcm_workers(),
      env_workers(),
      {APNS.Token, %{}},
      {Task.Supervisor, name: Pigeon.Tasks}
    ]
    |> List.flatten()
  end

  defp env_workers do
    case Application.get_env(:pigeon, :workers) do
      nil ->
        []

      workers ->
        Enum.flat_map(workers, fn {mod, fun} ->
          mod
          |> apply(fun, [])
          |> List.wrap()
          |> Enum.map(&worker/1)
          |> Enum.filter(& &1)
        end)
    end
  end

  defp worker(config) do
    {Pigeon.Worker, config: config, id: config.name}
  end

  defp apns_workers do
    workers_for(:apns, &APNS.ConfigParser.parse/1, Pigeon.Worker)
  end

  defp fcm_workers do
    workers_for(:fcm, &FCM.Config.new/1, Pigeon.Worker)
  end

  defp workers_for(name, config_fn, mod) do
    case Application.get_env(:pigeon, name) do
      nil ->
        []

      workers ->
        Enum.map(workers, fn {worker_name, _config} ->
          config = config_fn.(worker_name)
          {mod, config: config, id: config.name}
        end)
    end
  end
end
