defmodule Pigeon.Application do
  @moduledoc false

  use Application
  alias Pigeon.APNS
  alias Pigeon.Http2.Client

  @doc false
  def start(_type, _args) do
    Client.default().start
    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(workers(), opts)
  end

  defp workers do
    [
      {APNS.Token, %{}},
      {Task.Supervisor, name: Pigeon.Tasks}
    ]
    |> List.flatten()
  end
end
