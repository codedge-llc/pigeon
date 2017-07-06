defmodule Pigeon.APNS.WorkerTest do
  use ExUnit.Case

  alias Pigeon.APNS

  describe "push_uri/1" do
    test ":dev returns api.development.push.apple.com" do
      assert APNS.Worker.push_uri(:dev) == "api.development.push.apple.com"
    end

    test ":prod returns api.development.push.apple.com" do
      assert APNS.Worker.push_uri(:prod) == "api.push.apple.com"
    end

    test "anything else throws error" do
      :whatever
      |> APNS.Worker.push_uri()
      |> catch_error()
    end
  end

  describe "initialize_worker/1" do
    test "returns {:ok, config} on successful initialization" do
      result =
        :apns_default
        |> APNS.Config.config
        |> APNS.Worker.initialize_worker
      {:ok, %{
        apns_socket: _socket,
        mode: mode,
        reconnect: true,
        config: config,
        stream_id: stream_id,
        queue: _queue
      }} = result

      assert mode == :dev
      assert config == APNS.Config.config(:apns_default)
      assert stream_id == 1
    end

    test "returns {:stop, {:error, :invalid_config}} if certificate or key are invalid" do
      apns = Application.get_env(:pigeon, :apns)
      bad_config = %{apns[:apns_default] | cert: "bad_cert.pem"}
      bad_apns = Keyword.put(apns, :default, bad_config)

      Application.put_env(:pigeon, :apns, bad_apns)

      result =
        :default
        |> APNS.Config.config
        |> APNS.Worker.initialize_worker

      assert result == {:stop, {:error, :invalid_config}}

      Application.put_env(:pigeon, :apns, apns)
    end
  end

  describe "connect_socket_options/2" do
    test "returns {:ok, options} for valid config" do
      cert = "cert.pem"
      key = "key.pem"
      config = %{
        cert: cert,
        key: key
      }
      actual = APNS.Worker.connect_socket_options(config)
      expected = {:ok, [{:cert, cert},
                  {:key, key},
                  {:password, ''},
                  {:packet, 0},
                  {:reuseaddr, true},
                  {:active, true},
                  :binary]}

      assert actual == expected
    end

    test "includes {:port, 2197} if env apns_2197: true" do
      cert = "cert.pem"
      key = "key.pem"
      config = %{
        cert: cert,
        key: key,
        use_2197: true
      }
      actual = APNS.Worker.connect_socket_options(config)
      expected = {:ok, [{:cert, cert},
                  {:key, key},
                  {:password, ''},
                  {:packet, 0},
                  {:reuseaddr, true},
                  {:active, true},
                  :binary,
                  {:port, 2197}]}

      assert actual == expected
    end
  end
end
