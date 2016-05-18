defmodule Pigeon.HTTP2Test do
  use ExUnit.Case
  alias Pigeon.{HTTP2}

  test "push_uri" do
    assert HTTP2.push_uri(:prod) == 'api.push.apple.com'
    assert HTTP2.push_uri(:dev) == 'api.development.push.apple.com'
  end

  test "push_port" do
    assert HTTP2.push_port == 2197
  end

  test "connection_preface" do
    assert HTTP2.connection_preface == "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
  end

  test "settings_frame" do
    assert HTTP2.settings_frame == <<0, 0, 0, 4, 0, 0, 0, 0, 0>>
  end

  test "settings_ack_frame" do
    assert HTTP2.settings_ack_frame == <<0, 0, 0, 4, 1, 0, 0, 0, 0>>
  end

  test "encode_header" do
    expected_result = <<16, 10, 97, 112, 110, 115, 45, 116, 111, 112, 105, 99, 11, 99, 111, 109, 46, 84, 101, 115, 116, 105, 110, 103>>
    assert HTTP2.encode_header("apns-topic", "com.Testing") == expected_result
  end

  test "post_header" do
    assert HTTP2.post_header == <<1::1, 0::1, 0::1, 0::1, 0::1, 0::1, 1::1, 1::1>>
  end

  test "https_header" do
    assert HTTP2.https_header == <<1::1, 0::1, 0::1, 0::1, 0::1, 1::1, 1::1, 1::1>>
  end
end
