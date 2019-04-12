use Mix.Config

config :pigeon, :fcm,
  fcm_default: %{
    key: {:system, :string, "GCM_KEY"}
  }

config :pigeon, :apns,
  apns_default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  }

config :pigeon, :adm,
  adm_default: %{
    client_id: {:system, :string, "ADM_OAUTH2_CLIENT_ID"},
    client_secret: {:system, :string, "ADM_OAUTH2_CLIENT_SECRET"}
  }
