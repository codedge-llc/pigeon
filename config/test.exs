use Mix.Config

#config :pigeon, http2_client: Pigeon.Http2.Client.Chatterbox

config :pigeon, :test,
  fcm_key: System.get_env("GCM_KEY"),
  valid_fcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  apns_cert: "cert.pem",
  apns_key: "key_unencrypted.pem",
  apns_topic: System.get_env("APNS_TOPIC")

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
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
