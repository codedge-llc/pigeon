defmodule Pigeon.ADM.ResultParserTest do
  use ExUnit.Case
  doctest Pigeon.ADM.ResultParser, import: true

  test "parses known error reasons without crashing" do
    n = Pigeon.ADM.Notification.new("test")

    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidRegistrationId"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidData"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidConsolidationKey"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidExpiration"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidChecksum"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "InvalidType"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "Unregistered"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "AccessTokenExpired"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "MessageTooLarge"})
    Pigeon.ADM.ResultParser.parse(n, %{"reason" => "MaxRateExceeded"})
  end
end
