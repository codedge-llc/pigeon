use Mix.Config

config :pigeon, :gcm,
  key: System.get_env("GCM_KEY")

config :pigeon, :apns,
  default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  }

config :pigeon, :adm,
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
