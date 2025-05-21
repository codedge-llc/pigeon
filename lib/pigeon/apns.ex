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
            queue: Pigeon.HTTP.RequestQueue.new(),
            socket: nil

  @behaviour Pigeon.Adapter

  import Pigeon.Tasks, only: [process_on_response: 1]

  alias Pigeon.Configurable
  alias Pigeon.APNS.{ConfigParser, Error}
  alias Pigeon.HTTP.{Request, RequestQueue}

  require Logger

  @impl true
  def init(opts) do
    config = ConfigParser.parse(opts)
    Configurable.validate!(config)

    state = %__MODULE__{config: config}

    case Configurable.connect(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:ok, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_push(notification, state) do
    %{config: config, queue: queue, socket: socket} = state

    headers = Configurable.push_headers(config, notification, [])
    payload = Configurable.push_payload(config, notification, [])
    method = "POST"
    path = "/3/device/#{notification.device_token}"

    {:ok, socket, ref} =
      Mint.HTTP.request(socket, method, path, headers, payload)

    new_q = RequestQueue.add(queue, ref, notification)

    state =
      state
      |> Map.put(:socket, socket)
      |> Map.put(:queue, new_q)

    {:noreply, state}
  end

  @impl true
  def handle_info(:ping, %{socket: socket} = state) do
    {:ok, socket, _ref} = Mint.HTTP2.ping(socket)
    Configurable.schedule_ping(state.config)

    {:noreply, %{state | socket: socket}}
  end

  def handle_info({:closed, _}, %{config: config} = state) do
    case Configurable.connect(config) do
      {:ok, socket} ->
        Configurable.schedule_ping(config)
        {:noreply, %{state | socket: socket}}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  def handle_info(msg, state) do
    Pigeon.HTTP.handle_info(msg, state, &handle_response/1)
  end

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

  @spec get_header([{String.t(), String.t()}], String.t()) :: String.t() | nil
  def get_header(headers, key) do
    case Enum.find(headers, fn {k, _val} -> k == key end) do
      {^key, val} -> val
      nil -> nil
    end
  end
end
