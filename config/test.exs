use Mix.Config

config :pigeon, :test,
  fcm_key: {:system, :string, "GCM_KEY"},
  valid_fcm_reg_id: {:system, :string, "VALID_GCM_REG_ID"},
  valid_apns_token: {:system, :string, "VALID_APNS_TOKEN"},
  apns_cert: "cert.pem",
  apns_key: "key_unencrypted.pem",
  apns_topic: {:system, :string, "APNS_TOPIC"}

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
    key: {:system, :string, "GCM_KEY"}
  }

config :pigeon, :apns,
  apns_default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev
  },
  apns_jwt_static: %{
    key: "AuthKey.p8",
    key_identifier: {:system, :string, "APNS_JWT_KEY_IDENTIFIER"},
    team_id: {:system, :string, "APNS_JWT_TEAM_ID"},
    mode: :dev
  }

config :pigeon, :adm,
  adm_default: %{
    client_id: {:system, :string, "ADM_OAUTH2_CLIENT_ID"},
    client_secret: {:system, :string, "ADM_OAUTH2_CLIENT_SECRET"}
  }
