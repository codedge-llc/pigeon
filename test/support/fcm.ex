defmodule PigeonTest.FCM do
  @moduledoc false
  use Pigeon.Dispatcher, otp_app: :pigeon
end

defmodule PigeonTest.LegacyFCM do
  @moduledoc false
  use Pigeon.Dispatcher, otp_app: :pigeon
end
