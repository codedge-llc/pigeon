use Mix.Config

config :pigeon, 
  apns_mode: :dev,
  apns_cert: System.get_env("APNS_CERT"),
  apns_key: System.get_env("APNS_CERT_KEY"),
  gcm_key: System.get_env("GCM_KEY"),
  valid_gcm_reg_id: System.get_env("VALID_GCM_REG_ID")
