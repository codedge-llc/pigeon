use Mix.Config

config :pigeon, :test,
  valid_gcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  apns_cert: "cert.pem",
  apns_key: "key_unencrypted.pem",
  apns_topic: System.get_env("APNS_TOPIC"),
  apns_mode: :dev

config :pigeon, :gcm,
  key: System.get_env("GCM_KEY")

config :pigeon, :apns,
  default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :prod,
    pool_size: 2,
    max_overflow: 0
  }
