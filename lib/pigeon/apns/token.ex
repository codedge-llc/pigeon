defmodule Pigeon.APNS.Token do
  @moduledoc false
  import Joken.Config

  alias Pigeon.APNS.JWTConfig

  require Logger

  # seconds - 10 seconds short of one hour
  @token_max_age 3_590

  # 5 seconds
  @shutdown_timeout 5_000

  @spec start_link((-> any())) :: Agent.on_start()
  def start_link(_) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: @shutdown_timeout
    }
  end

  @spec get(JWTConfig.t()) :: String.t()
  def get(%JWTConfig{} = config) do
    token_storage_key = storage_key(config)

    Agent.get_and_update(__MODULE__, fn map ->
      {timestamp, saved_token} = Map.get(map, token_storage_key, {0, nil})
      now = :os.system_time(:seconds)

      age = now - timestamp

      if age < @token_max_age do
        {saved_token, map}
      else
        token = generate_apns_jwt(config)
        {token, Map.put(map, token_storage_key, {now, token})}
      end
    end)
  end

  @spec storage_key(JWTConfig.t()) :: String.t()
  defp storage_key(config) do
    "#{config.key_identifier}:#{config.team_id}:#{config.uri}"
  end

  @spec generate_apns_jwt(JWTConfig.t()) :: String.t()
  defp generate_apns_jwt(config) do
    %{key: key, key_identifier: key_identifier, team_id: team_id} = config

    key = %{"pem" => key}
    now = :os.system_time(:seconds)

    signer =
      Joken.Signer.create("ES256", key, %{"kid" => key_identifier})

    [iss: team_id, iat: now]
    |> default_claims()
    |> Joken.generate_and_sign!(%{}, signer)
  end
end
