use Mix.Config

config :pigeon, :fcm,
  fcm_default: %{
    key: System.get_env("GCM_KEY")
  }

config :pigeon, :apns,
  apns_default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  }

config :pigeon, :adm,
  adm_default: %{
    client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
    client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
  }
