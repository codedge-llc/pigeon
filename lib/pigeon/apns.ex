defmodule Pigeon.APNS do
  @moduledoc """
  `Pigeon.Adapter` for Apple Push Notification Service (APNS) push notifications.

  ## Getting Started

  ### Create a dispatcher.

  ```
  # lib/your_app/apns.ex

  defmodule YourApp.APNS do
    use Pigeon.Dispatcher, otp_app: :your_app
  end
  ```

  ### Configure your dispatcher.

  Configure your `APNS` dispatcher and start it on application boot.

  ```
  # config.exs

  config :your_app, YourApp.APNS,
    adapter: Pigeon.APNS,
    cert: File.read!("cert.pem"),
    key: File.read!("key_unencrypted.pem"),
    mode: :dev

  # Or for token based authentication:

  config :your_app, YourApp.APNS,
    adapter: Pigeon.APNS,
    key: File.read!("AuthKey.p8"),
    key_identifier: "ABC1234567",
    mode: :dev,
    team_id: "DEF8901234"
  ```

  Add it to your supervision tree.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        YourApp.APNS
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end
  end
  ```

  If preferred, you can include your configuration directly.

  ```
  defmodule YourApp.Application do
    @moduledoc false

    use Application

    @doc false
    def start(_type, _args) do
      children = [
        {YourApp.APNS, apns_opts()}
      ]
      opts = [strategy: :one_for_one, name: YourApp.Supervisor]
      Supervisor.start_link(children, opts)
    end

    defp apns_opts do
      [
        adapter: Pigeon.APNS,
        cert: File.read!("cert.pem"),
        key: File.read!("key_unencrypted.pem"),
        mode: :dev
      ]
    end
  end
  ```

  ### Create a notification.

  ```
  n = Pigeon.APNS.Notification.new("your message", "your device token", "your push topic")
  ```

  > #### Note {: .info}
  >
  > Note: Your push topic is generally the app's bundle identifier.

  ### Send the notification.

  Pushes are synchronous and return the notification with an updated `:response` key.

  If the dispatcher has no live connection to APNS, `:response` is `:connection_error`
  and the push was not sent, so it is safe to resend. If the connection is lost after
  the push was sent, `:response` is `:timeout` and delivery is unknown.

  ```
  YourApp.APNS.push(n)
  ```

  ## Configuration Options

  #### Certificate Authentication

  - `:cert` - Push certificate. Must be the full-text string of the file contents.
  - `:key` - Push private key. Must be the full-text string of the file contents.

  #### Token Authentication

  - `:key` - JWT private key. Must be the full-text string of the file contents.
  - `:key_identifier` - A 10-character key identifier (kid) key, obtained from
    your developer account.
  - `:team_id` - Your 10-character Team ID, obtained from your developer account.

  #### Shared Options

  - `:mode` - If set to `:dev` or `:prod`, will set the appropriate `:uri`.
  - `:ping_period` - Interval between server pings. Necessary to keep long
    running APNS connections alive. Defaults to 10 minutes.
  - `:port` - Push server port. Can be any value, but APNS only accepts
    `443` and `2197`.
  - `:uri` - Push server uri. If set, overrides uri defined by `:mode`.
    Useful for test environments.

  ## Generating Your Certificate and Key .pem

  1. In Keychain Access, right-click your push certificate and select _"Export..."_
  2. Export the certificate as `cert.p12`
  3. Click the dropdown arrow next to the certificate, right-click the private
     key and select _"Export..."_
  4. Export the private key as `key.p12`
  5. From a shell, convert the certificate.

  ```
  openssl pkcs12 -legacy -clcerts -nokeys -out cert.pem -in cert.p12
  ```

  6. Convert the key. Be sure to set a PEM pass phrase here. The pass phrase must be 4 or
     more characters in length or this will not work. You will need that pass phrase added
     here in order to remove it in the next step.

  ```
  openssl pkcs12 -legacy -nocerts -out key.pem -in key.p12
  ```

  7. Remove the PEM pass phrase from the key.

  ```
  openssl rsa -in key.pem -out key_unencrypted.pem
  ```

  8. `cert.pem` and `key_unencrypted.pem` can now be used in your configuration.
  """

  defstruct config: nil,
            conn: nil

  @behaviour Pigeon.Adapter

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.APNS.{ConfigParser, Error}
  alias Pigeon.Configurable
  alias Pigeon.HTTP.{Connection, Request}

  @impl true
  def init(opts) do
    config = ConfigParser.parse(opts)
    Configurable.validate!(config)

    conn =
      Connection.new(
        __MODULE__,
        fn -> Configurable.connect(config) end,
        Configurable.ping_period(config)
      )

    {:ok, %__MODULE__{config: config, conn: conn}}
  end

  @impl true
  def handle_push(notification, %{config: config, conn: conn} = state) do
    headers = Configurable.push_headers(config, notification, [])
    payload = Configurable.push_payload(config, notification, [])
    path = "/3/device/#{notification.device_token}"

    case Connection.request(conn, "POST", path, headers, payload, notification) do
      {:ok, conn} ->
        {:noreply, %{state | conn: conn}}

      {:error, conn, _reason} ->
        notification
        |> Map.put(:response, :connection_error)
        |> process_on_response()

        {:noreply, %{state | conn: conn}}
    end
  end

  @impl true
  def handle_info(msg, %{conn: conn} = state) do
    case Connection.handle_message(conn, msg, &handle_response/1) do
      {:ok, conn} -> {:noreply, %{state | conn: conn}}
      :unknown -> {:noreply, state}
    end
  end

  @doc false
  @spec handle_response(Request.t()) :: :ok
  def handle_response(%{status: 200} = request) do
    %{headers: headers, notification: notification} = request

    notification
    |> Map.put(:id, get_header(headers, "apns-id"))
    |> Map.put(:response, :success)
    |> process_on_response()
  end

  def handle_response(request) do
    %{body: body, notification: notification} = request

    notification
    |> Map.put(:response, Error.parse(body))
    |> process_on_response()
  end

  @doc false
  @spec get_header([{String.t(), String.t()}], String.t()) :: String.t() | nil
  def get_header(headers, key) do
    case Enum.find(headers, fn {k, _val} -> k == key end) do
      {^key, val} -> val
      nil -> nil
    end
  end
end
