use Mix.Config

config :pigeon, :test,
  fcm_key: System.get_env("GCM_KEY"),
  valid_fcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  apns_cert: File.read!("cert.pem"),
  apns_key: File.read!("key_unencrypted.pem"),
  apns_topic: System.get_env("APNS_TOPIC")

config :pigeon,
  debug_log: true

config :pigeon, PigeonTest.ADM,
  adapter: Pigeon.ADM,
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")

config :pigeon, PigeonTest.APNS,
  adapter: Pigeon.APNS,
  cert: File.read!("cert.pem"),
  key: File.read!("key_unencrypted.pem"),
  mode: :dev

config :pigeon, PigeonTest.APNS.JWT,
  adapter: Pigeon.APNS,
  key: File.read!("AuthKey.p8"),
  key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_JWT_TEAM_ID"),
  mode: :dev

config :pigeon, PigeonTest.LegacyFCM,
  adapter: Pigeon.LegacyFCM,
  key: System.get_env("GCM_KEY")

config :pigeon, PigeonTest.FCM,
  adapter: Pigeon.FCM,
  project_id: System.get_env("FCM_PROJECT"),
  service_account_json: File.read!("service-account.json")

config :pigeon, PigeonTest.Sandbox, adapter: Pigeon.Sandbox
