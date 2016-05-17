use Mix.Config

config :pigeon,
  gcm_key: System.get_env("GCM_KEY"),
  valid_gcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  apns_port: 2195
