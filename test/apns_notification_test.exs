defmodule Pigeon.APNSNotificationTest do
  use ExUnit.Case

  def test_device_token, do: "test1234"
  def test_msg, do: "test"
  def test_topic, do: "topic.Test"

  test "new" do
    expected_result = %Pigeon.APNS.Notification{
      device_token: test_device_token,
      topic: test_topic,
      payload: %{"aps" => %{"alert" => test_msg}},
      expiration: nil
    }
    assert Pigeon.APNS.Notification.new(test_msg, test_device_token, test_topic) == expected_result
  end

  test "put_alert" do
    title = "Test Title"
    body = "test body"
    alert = %{"title" => title, "body" => body}

    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_alert(alert)

    assert n.payload == %{"aps" => %{"alert" => alert}}
  end

  test "put_badge" do
    badge = 5
    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_badge(badge)

    assert n.payload == %{"aps" => %{"alert" => test_msg, "badge" => badge}}
  end

  test "put_sound" do
    sound = "default"
    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_sound(sound)

    assert n.payload == %{"aps" => %{"alert" => test_msg, "sound" => sound}}
  end

  test "put_content_available" do
    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_content_available

    assert n.payload == %{"aps" => %{"alert" => test_msg, "content-available" => 1}}
  end

  test "put_category" do
    category = "test-category"
    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_category(category)

    assert n.payload == %{"aps" => %{"alert" => test_msg, "category" => category}}
  end

  test "put_custom" do
    custom = %{"custom-key" => %{"custom-value" => 500}}
    n =
      test_msg
      |> Pigeon.APNS.Notification.new(test_device_token, test_topic)
      |> Pigeon.APNS.Notification.put_custom(custom)

    assert n.payload ==
      %{"aps" => %{"alert" => test_msg}, "custom-key" => %{"custom-value" => 500}}
  end
end
