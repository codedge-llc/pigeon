defmodule PigeonTest.FCM do
  @moduledoc false
  use Pigeon.Dispatcher, otp_app: :pigeon
end

defmodule PigeonTest.LegacyFCM do
  @moduledoc false
  use Pigeon.Dispatcher, otp_app: :pigeon
end

defmodule PigeonTest.GothHttpClient.Stub do
  @moduledoc """
  A collection of functions that can be used as custom `:http_client` values. Used to avoid
  calling out to GCP during tests.
  """

  @doc """
  Always returns a stub access_token response, as if being requested of a Google Metadata Server

  ## Usage
  ```
  goth_opts = [
    name: PigeonTest.Goth,
    source: {:metadata, []}
    http_client: {&PigeonTest.GothHttpClient.Stub.access_token_response/1, []}
  ]

  fcm_opts = [
    adapter: Pigeon.Sandbox,
    project_id: "example-123",
    goth: PigeonTest.Goth
  ]

  children = [
    {Goth, goth_opts}
    {PigeonTest.FCM, fcm_opts}
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
  """
  @spec access_token_response(keyword()) ::
          {:ok,
           %{
             status: pos_integer(),
             headers: list(),
             body: String.t()
           }}
  def access_token_response(_) do
    body = %{
      "access_token" => "FAKE_APPLICATION_DEFAULT_CREDENTIALS_ACCESS_TOKEN",
      "expires_in" => :timer.minutes(30),
      "token_type" => "Bearer"
    }

    {:ok, %{status: 200, headers: [], body: Jason.encode!(body)}}
  end
end
