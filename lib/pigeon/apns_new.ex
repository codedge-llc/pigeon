defmodule Pigeon.APNSNew do
  @behaviour Pigeon.Adapter

  @impl true
  def initial_state(opts) do
    config = APNS.ConfigParser.parse(opts)
    Configurable.validate!(config)

    %{
      config: config,
      access_token: nil,
      access_token_refreshed_datetime_erl: {{0, 0, 0}, {0, 0, 0}},
      access_token_expiration_seconds: 0,
      access_token_type: nil
    }
  end

  @impl true
  def handle_push(_notification, _on_response, state) do
    state
  end

  @impl true
  def handle_info({_from, {:ok, %HTTPoison.Response{status_code: 200}}}, state) do
    {:noreply, state}
  end
end
