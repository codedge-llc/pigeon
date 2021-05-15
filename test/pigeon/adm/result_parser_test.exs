defmodule Pigeon.ADM.ResultParserTest do
  use ExUnit.Case
  doctest Pigeon.ADM.ResultParser, import: true

  test "parses known error reasons without crashing" do
    n = Pigeon.ADM.Notification.new("test")
    onr = fn _ -> :ok end

    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidRegistrationId"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidData"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidConsolidationKey"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidExpiration"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidChecksum"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidType"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "Unregistered"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "AccessTokenExpired"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "MessageTooLarge"}, onr)
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "MaxRateExceeded"}, onr)
  end
end
