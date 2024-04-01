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


  ## Usage
  ```
  # lib/your_app/goth.ex
  defmodule YourApp.Goth

    @spec child_spec(any()) :: Supervisor.child_spec()
    def child_spec(_args) do
      env_opts = Keyword.new(Application.get_env(:your_app, YourApp.Goth, []))
      opts = Keyword.merge([name: YourApp.Goth], env_opts)

      %{
        :id => YourApp.Goth,
        :start => {Goth, :start_link, [opts]}
      }
    end
  end

  # config/test.exs
  # Config for the Goth genserver, YourApp.Goth
  config :your_app, YourApp.Goth,
    source: {:metadata, []},
    http_client: {&PigeonTest.GothHttpClient.Stub.access_token_response/1, []}


  # application.exs
  def start(_type, _args) do
    children = [
      # The `child_spec/1` handles fetching the proper config
      YourApp.Goth,
      YourApp.FCM
    ]
    opts = [strategy: :one_for_one, name: YourApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
  ```
  """

  @doc """
  Always returns a stub access_token response, as if being requested of a Google Metadata Server.

  See module documentation for usage.
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
