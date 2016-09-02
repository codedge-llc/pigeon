use Mix.Config

config :pigeon, :gcm,
  key: System.get_env("GCM_KEY")

config :pigeon, :apns,
  default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev,
    pool_size: 2,
    max_overflow: 0
  }
