use Mix.Config

config :pigeon, :fcm,
  fcm_default: %{
    key: System.get_env("GCM_KEY")
  }

config :pigeon, :apns,
  apns_default: %{
    cert: File.read!("cert.pem"),
    key: File.read!("key_unencrypted.pem"),
    mode: :dev
  }

config :pigeon, :adm,
  adm_default: %{
    client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
    client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
  }

config :pigeon, PigeonTest.ADM,
  adapter: Pigeon.ADM,
  client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
  client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")

config :pigeon, PigeonTest.APNS,
  adapter: Pigeon.APNSNew,
  # cert: File.read!("cert.pem"),
  # key: File.read!("key_unencrypted.pem"),
  # mode: :dev
  key: File.read!("AuthKey.p8"),
  key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
  team_id: System.get_env("APNS_JWT_TEAM_ID"),
  mode: :dev

config :pigeon, PigeonTest.FCM,
  adapter: Pigeon.FCMNew,
  key: System.get_env("GCM_KEY")
