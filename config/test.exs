import Config

config :pigeon, :test,
  apns_cert: System.get_env("APNS_CERT"),
  apns_key: System.get_env("APNS_KEY_UNENCRYPTED"),
  apns_topic: System.get_env("APNS_TOPIC"),
  fcm_key: System.get_env("GCM_KEY"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  valid_fcm_reg_id: System.get_env("VALID_GCM_REG_ID")

config :pigeon, PigeonTest.APNS,
  adapter: Pigeon.APNS,
  key: System.get_env("APNS_KEY_UNENCRYPTED"),
  key_identifier: System.get_env("APNS_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_TEAM_ID"),
  mode: :prod

config :pigeon, PigeonTest.APNS.JWT,
  adapter: Pigeon.APNS,
  key: System.get_env("APNS_KEY_UNENCRYPTED"),
  key_identifier: System.get_env("APNS_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_TEAM_ID"),
  mode: :prod

config :pigeon, PigeonTest.FCM,
  adapter: Pigeon.FCM,
  project_id: System.get_env("FCM_PROJECT_ID"),
  service_account_json: System.get_env("FCM_SERVICE_ACCOUNT_JSON")

config :pigeon, PigeonTest.Sandbox, adapter: Pigeon.Sandbox
