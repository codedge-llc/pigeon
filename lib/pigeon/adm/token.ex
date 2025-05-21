defmodule Pigeon.ADM.Token do
  @moduledoc false

  # We expire the token slightly sooner than the actual expiration
  # to account for network latency and other factors.
  @token_refresh_early_seconds 5

  def expired?(_refreshed_datetime_erl, 0), do: true

  def expired?(refreshed_datetime_erl, expiration_seconds) do
    seconds_since(refreshed_datetime_erl) >=
      expiration_seconds - @token_refresh_early_seconds
  end

  defp seconds_since(datetime_erl) do
    gregorian_seconds =
      datetime_erl
      |> :calendar.datetime_to_gregorian_seconds()

    now_gregorian_seconds =
      :os.timestamp()
      |> :calendar.now_to_universal_time()
      |> :calendar.datetime_to_gregorian_seconds()

    now_gregorian_seconds - gregorian_seconds
  end

  @spec refresh_body(String.t(), String.t()) :: String.t()
  def refresh_body(client_id, client_secret) do
    %{
      "grant_type" => "client_credentials",
      "scope" => "messaging:push",
      "client_id" => client_id,
      "client_secret" => client_secret
    }
    |> URI.encode_query()
  end

  @spec refresh_headers :: [{String.t(), String.t()}]
  def refresh_headers do
    [{"Content-Type", "application/x-www-form-urlencoded;charset=UTF-8"}]
  end
end
