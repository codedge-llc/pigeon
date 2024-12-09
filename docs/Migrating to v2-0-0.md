# Migrating to v2.0

## Migrating from v2.0 RCs

See the [CHANGELOG](../CHANGELOG.md) for specific breaking changes from each RC. If upgrading from RC3,
no additional changes are required.

## Migrating from v1.6

1. Update your mix.exs dependencies.

```elixir
[
  {:pigeon, "~> 2.0"} # Change this
  {:kadadbra, ~> 0.6.0} # Remove this
]
```

2. Update configuration of your push workers.

Pigeon push workers are now started under your application supervision tree instead of
Pigeon.

Using APNS as an example, if you have a default push worker configured like this:

```elixir
config :pigeon, :apns,
  apns_default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  }
```

Remove the config and instead define a push worker like this:

```elixir
# lib/your_app/apns.ex

defmodule YourApp.APNS do
  use Pigeon.Dispatcher, otp_app: :your_app
end
```

```elixir
# config.exs

config :your_app, YourApp.APNS,
  adapter: Pigeon.APNS,
  cert: File.read!("cert.pem"),
  key: File.read!("key_unencrypted.pem"),
  mode: :dev
```

And add it to your supervision tree.

```elixir
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

Configuration of the worker can now be passed directly if desired.

```elixir
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

And that's it! See the rest of this guide for other breaking changes specific to your
push type.

### Breaking Changes for ADM

No addtional changes required apart from the configuration above.

### Breaking Changes for APNS

- `:certfile` and `:keyfile` are no longer valid options for APNS configurations.
  Instead, read the file before loading (e.g. `cert: File.read!("cert.pem")`)
  ([#183](https://github.com/codedge-llc/pigeon/pull/183))

### Breaking Changes for FCM

1. FCM's legacy API, used by Pigeon v1.6, has been decommissioned. To upgrade, add the legacy
   adapter to your mix.exs.

```elixir
def deps do
  [
    {:pigeon_legacy_fcm, "~> 0.1.0"}
  ]
end
```

2. Rename all instances of `Pigeon.FCM` in your project to `Pigeon.LegacyFCM`. For example,
   `Pigeon.FCM.Notification` becomes `Pigeon.LegacyFCM.Notification`.

Once renamed, your project should now compile again.

3. Upgrade to the new FCM v1 API. Follow the instructions in `Pigeon.FCM` to set up a new
   push worker and update your Pigeon FCM notifications accordingly.
