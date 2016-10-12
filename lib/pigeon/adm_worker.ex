defmodule Pigeon.ADMWorker do
  @moduledoc """
    Handles all Amazon ADM request and response parsing over an HTTP2 connection.
    Includes managing OAuth2 tokens.
  """
  use GenServer

  def start_link(name, config) do
    GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  end

  def stop, do: :gen_server.cast(self, :stop)

  def init({:ok, config}), do: initialize_worker(config)

  def initialize_worker(config) do
    {:ok, %{
      client_id: config[:client_id],
      client_secret: config[:client_secret]
    }}
  end
end