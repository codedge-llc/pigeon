use Mix.Config

config :pigeon, :gcm,
  key: System.get_env("GCM_KEY")

config :pigeon, :apns,
  default: %{
    cert: "cert.pem",
    key: "key_unencrypted.pem",
    mode: :dev,
    pool_size: 1,
    use_2197: true,
    max_overflow: 0
  }
