import Config

config :pigeon, :test,
  apns_cert: System.get_env("APNS_CERT"),
  apns_key: System.get_env("APNS_KEY_UNENCRYPTED"),
  apns_topic: System.get_env("APNS_TOPIC"),
  fcm_key: System.get_env("GCM_KEY"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  valid_fcm_reg_id: System.get_env("VALID_GCM_REG_ID")

config :pigeon, PigeonTest.ADM,
  adapter: Pigeon.ADM,
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")

config :pigeon, PigeonTest.APNS,
  adapter: Pigeon.APNS,
  cert: System.get_env("APNS_CERT"),
  key: System.get_env("APNS_KEY_UNENCRYPTED"),
  mode: :dev

config :pigeon, PigeonTest.APNS.JWT,
  adapter: Pigeon.APNS,
  key: System.get_env("APNS_AUTH_KEY_P8"),
  key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_JWT_TEAM_ID"),
  mode: :dev

config :pigeon, PigeonTest.LegacyFCM,
  adapter: Pigeon.LegacyFCM,
  key: System.get_env("GCM_KEY")

config :pigeon, PigeonTest.FCM,
  adapter: Pigeon.FCM,
  project_id: System.get_env("FCM_PROJECT"),
  token_fetcher: PigeonTest.Goth

config :pigeon, PigeonTest.Sandbox, adapter: Pigeon.Sandbox
