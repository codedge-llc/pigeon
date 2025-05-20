defmodule Pigeon.Application do
  @moduledoc false

  use Application
  alias Pigeon.APNS

  @doc false
  def start(_type, _args) do
    children = [
      Pigeon.Registry,
      {APNS.Token, %{}},
      {Task.Supervisor, name: Pigeon.Tasks}
    ]

    opts = [strategy: :one_for_one, name: :pigeon]
    Supervisor.start_link(children, opts)
  end
end
