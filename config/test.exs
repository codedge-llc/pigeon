use Mix.Config

config :pigeon,
  gcm_key: System.get_env("GCM_KEY"),
  valid_gcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  apns_cert: "cert.pem",
  apns_key: "key_unencrypted.pem",
  apns_topic: System.get_env("APNS_TOPIC"),
  apns_mode: :dev
