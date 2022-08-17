defmodule Pigeon.APNS.Token do
  @moduledoc false
  import Joken.Config

  alias Pigeon.APNS.JWTConfig

  # seconds - 10 seconds short of one hour
  @token_max_age 3_590

  @type t :: {non_neg_integer(), binary() | nil}

  @spec start_link((() -> any())) :: Agent.on_start()
  def start_link(_) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5_000
    }
  end

  @spec get(JWTConfig.t()) :: t
  def get(%JWTConfig{} = config) do
    token_storage_key = config.key_identifier <> ":" <> config.team_id

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

  @spec generate_apns_jwt(JWTConfig.t()) :: binary
  defp generate_apns_jwt(config) do
    key = %{"pem" => config.key}
    now = :os.system_time(:seconds)

    signer =
      Joken.Signer.create("ES256", key, %{"kid" => config.key_identifier})

    {:ok, token, _claims} =
      default_claims(iss: config.team_id, iat: now)
      |> Joken.generate_and_sign(%{}, signer)

    token
  end
end
