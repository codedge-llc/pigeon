defmodule Pigeon.TestConfig do
  @moduledoc false

  alias Pigeon.{APNS, FCM, ADM}

  def apns_dynamic do
    APNS.Config.new(
      name: :apns_dynamic,
      cert: File.read!("cert.pem"),
      key: File.read!("key_unencrypted.pem"),
      mode: :dev
    )
  end

  def apns_jwt_dynamic do
    APNS.JWTConfig.new(
      name: :apns_jwt_dynamic,
      key: File.read!("AuthKey.p8"),
      key_identifier: System.get_env("APNS_JWT_KEY_IDENTIFIER"),
      team_id: System.get_env("APNS_JWT_TEAM_ID"),
      mode: :dev
    )
  end

  def fcm_dynamic do
    FCM.Config.new(
      name: :fcm_dynamic,
      key: System.get_env("GCM_KEY")
    )
  end

  def adm_dynamic do
    ADM.Config.new(
      name: :adm_dynamic,
      client_id: System.get_env("ADM_OAUTH2_CLIENT_ID"),
      client_secret: System.get_env("ADM_OAUTH2_CLIENT_SECRET")
    )
  end
end
