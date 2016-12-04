defmodule Pigeon.APNSD do
  require Logger
  import Supervisor.Spec

  import Pigeon.APNS, only: [group_responses: 1]
  @default_timeout 5_000

  def push(notifications, opts)  when is_list(notifications) do
    worker_pid = ensure_worker(opts[:cert])

    case opts[:on_response] do
      nil ->
        tasks = for n <- notifications, do: Task.async(fn -> do_sync_push(worker_pid, n) end)
        tasks
        |> Task.yield_many(@default_timeout + 500)
        |> Enum.map(fn {task, response} -> response || Task.shutdown(task, :brutal_kill) end)
        |> group_responses()
      on_response -> push(worker_pid, notifications, on_response, opts)
    end
  end
  def push(notification, opts) do
    worker_pid = ensure_worker(opts[:cert])

    case opts[:on_response] do
      nil -> do_sync_push(worker_pid, notification)
      on_response -> push(worker_pid, notification, on_response, opts)
    end
  end

  def push(worker_pid, notifications, on_response) when is_list(notifications) do
    for n <- notifications, do: push(worker_pid, n, on_response, opts)
  end
  def push(worker_pid, notification, on_response) do
    GenServer.cast(worker_pid, {:push, :apns, notification, on_response})
  end

  def do_sync_push(worker_pid, notification) do
    pid = self
    on_response = fn(x) -> send pid, {:ok, x} end
    GenServer.cast(worker_pid, {:push, :apns, notification, on_response})

    receive do
      {:ok, x} -> x
    after
      @default_timeout -> {:error, :timeout, notification}
    end
  end

  def ensure_worker(full_cert) do
    cert_hash =
      :crypto.hash(:sha, full_cert)
      |> Base.encode16()

    [{:Certificate, cert_der, _},
     {:PrivateKeyInfo, key_der, _}
    ] = :public_key.pem_decode(full_cert)

    config =
      %{name: cert_hash,
        mode: Application.get_env(:apnsd, :env, :dev),
        key: {:PrivateKeyInfo, key_der},
        keyfile: :nil,
        cert: cert_der,
        certfile: :nil,
        dynamic: true
      }

    case cert_hash |> String.to_atom() |> GenServer.whereis() do
      :nil ->
        {:ok, pid} =
          Supervisor.start_child(Pigeon.APNSD.Supervisor, [config])
        pid
      pid ->
        pid
    end
  end
end

defmodule Pigeon.APNSD.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def init([]) do
    children = [ worker(Pigeon.APNSWorker, [], restart: :transient) ]
    supervise(children, strategy: :simple_one_for_one)
  end
end
