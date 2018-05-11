use Mix.Config

config :pigeon, :test,
  fcm_key: System.get_env("GCM_KEY"),
  valid_fcm_reg_id: System.get_env("VALID_GCM_REG_ID"),
  valid_apns_token: System.get_env("VALID_APNS_TOKEN"),
  apns_cert: "cert.pem",
  apns_key: "key_unencrypted.pem",
  apns_topic: System.get_env("APNS_TOPIC")

config :pigeon,
  debug_log: true,
  workers: [
    {Pigeon.TestConfig, :apns_dynamic},
    {Pigeon.TestConfig, :apns_jwt_dynamic},
    {Pigeon.TestConfig, :fcm_dynamic},
    {Pigeon.TestConfig, :adm_dynamic}
  ]

config :pigeon, :fcm,
  fcm_default: %{
    key: System.get_env("GCM_KEY")
  }

config :pigeon, :apns,
  apns_default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  },
  apns_jwt_static: %{
    key: "AuthKey.p8",
    key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
    team_id: System.get_env("APNS_JWT_TEAM_ID"),
    mode: :dev
  }

config :pigeon, :adm,
  adm_default: %{
    client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
    client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
  }
