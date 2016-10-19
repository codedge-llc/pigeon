use Mix.Config

config :pigeon,
  apns_mode: :dev,
  apns_cert: System.get_env("APNS_CERT"),
  apns_key: System.get_env("APNS_CERT_KEY"),
  gcm_key: System.get_env("GCM_KEY"),
  adm_client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  adm_client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
