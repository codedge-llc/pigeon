use Mix.Config

config :pigeon, PigeonTest.ADM,
  adapter: Pigeon.ADM,
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")

config :pigeon, PigeonTest.APNS,
  adapter: Pigeon.APNS,
  key: System.get_env("APNS_AUTH_KEY_P8"),
  key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_JWT_TEAM_ID"),
  mode: :dev

config :pigeon, PigeonTest.FCM,
  adapter: Pigeon.FCM,
  project_id: System.get_env("FCM_PROJECT"),
  service_account_json: System.get_env("FCM_SERVICE_ACCOUNT_JSON")
