defmodule Pigeon.APNSWorkerTest do
  use ExUnit.Case

  alias Pigeon.{APNS, APNSWorker}

  describe "push_uri/1" do
    test ":dev returns api.development.push.apple.com" do
      assert APNSWorker.push_uri(:dev) == "api.development.push.apple.com"
    end

    test ":prod returns api.development.push.apple.com" do
      assert APNSWorker.push_uri(:prod) == "api.push.apple.com"
    end

    test "anything else throws error" do
      :whatever
      |> APNSWorker.push_uri()
      |> catch_error()
    end
  end

  describe "initialize_worker/1" do
    test "returns {:ok, config} on successful initialization" do
      result =
        APNS.Config.default_config
        |> APNSWorker.initialize_worker
      {:ok, %{
        apns_socket: _socket,
        mode: mode,
        config: config,
        stream_id: stream_id,
        queue: _queue
      }} = result

      assert mode == :dev
      assert config == APNS.Config.default_config
      assert stream_id == 1
    end

    test "returns {:stop, {:error, :invalid_config}} if certificate or key are invalid" do
      cert = Application.get_env(:pigeon, :apns_cert)
      Application.put_env(:pigeon, :apns_cert, "bad_cert.pem")

      result =
        APNS.Config.default_config
        |> APNSWorker.initialize_worker

      assert result == {:stop, {:error, :invalid_config}}

      Application.put_env(:pigeon, :apns_cert, cert)
    end
  end

  describe "connect_socket_options/2" do
    test "returns valid socket options for given cert and key" do
      cert = {:cert, "cert.pem"}
      key = {:key, "key.pem"}
      actual = APNSWorker.connect_socket_options(cert, key)
      expected = [cert,
                  key,
                  {:password, ''},
                  {:packet, 0},
                  {:reuseaddr, false},
                  {:active, true},
                  :binary]

      assert actual == expected
    end

    test "includes {:port, 2197} if env apns_2197: true" do
      port = Application.get_env(:pigeon, :apns_2197)
      Application.put_env(:pigeon, :apns_2197, true)

      cert = {:cert, "cert.pem"}
      key = {:key, "key.pem"}
      actual = APNSWorker.connect_socket_options(cert, key)
      expected = [cert,
                  key,
                  {:password, ''},
                  {:packet, 0},
                  {:reuseaddr, false},
                  {:active, true},
                  :binary,
                  {:port, 2197}]

      assert actual == expected

      Application.put_env(:pigeon, :apns_2197, port)
    end
  end
end
