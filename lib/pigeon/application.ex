defmodule Pigeon.Application do
  @moduledoc false

  use Application
  alias Pigeon.APNS
  alias Pigeon.Http2.Client

  @doc false
  def start(_type, _args) do
    Client.default().start()

    children = [
      Pigeon.Registry,
      {APNS.Token, %{}},
      {Task.Supervisor, name: Pigeon.Tasks}
    ]

    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(children, opts)
  end
end
