defmodule PigeonTest.FCM do
  use Pigeon.Dispatcher, otp_app: :pigeon
end

defmodule PigeonTest.LegacyFCM do
  use Pigeon.Dispatcher, otp_app: :pigeon
end
