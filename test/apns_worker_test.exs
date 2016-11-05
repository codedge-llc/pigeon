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
        :default
        |> APNS.Config.config
        |> APNSWorker.initialize_worker
      {:ok, %{
        apns_socket: _socket,
        mode: mode,
        config: config,
        stream_id: stream_id,
        queue: _queue
      }} = result

      assert mode == :prod
      assert config == APNS.Config.config(:default)
      assert stream_id == 1
    end

    test "returns {:stop, {:error, :invalid_config}} if certificate or key are invalid" do
      apns = Application.get_env(:pigeon, :apns)
      bad_config = %{ apns[:default] | cert: "bad_cert.pem"}
      bad_apns = Keyword.put(apns, :default, bad_config)

      Application.put_env(:pigeon, :apns, bad_apns)

      result =
        :default
        |> APNS.Config.config
        |> APNSWorker.initialize_worker

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
      actual = APNSWorker.connect_socket_options(config)
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
      actual = APNSWorker.connect_socket_options(config)
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

  describe "ping/1" do
    test "sends ping frame" do
      result =
        :default
        |> APNS.Config.config
        |> APNSWorker.initialize_worker
      {:ok, %{
        apns_socket: socket,
        mode: mode,
        config: config,
        stream_id: stream_id,
        queue: _queue
      }} = result
      
      Pigeon.APNSWorker.ping(socket)
    end
  end
end
