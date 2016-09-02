defmodule Pigeon.APNS.ConfigTest do
  use ExUnit.Case

  alias Pigeon.APNS.Config

  test "valid? returns true if proper ssl config keys present" do
    assert Config.valid?(Config.config(:default))
  end
end
