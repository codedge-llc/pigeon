defmodule Pigeon.ADM.ConfigTest do
  use ExUnit.Case

  alias Pigeon.ADM.Config

  test "valid? returns true if proper Amazon ADM config keys present" do
    assert Config.valid?(Config.config(:adm_default))
  end
end
