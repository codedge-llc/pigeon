defmodule Pigeon.GCMWorker do
  @moduledoc """
    Handles all APNS request and response parsing over an HTTP2 connection.
  """
  use GenServer
  require Logger

  alias Pigeon.GCM.NotificationResponse

  def default_name, do: :gcm_default

  defp gcm_uri(config),   do: config[:endpoint] || "fcm.googleapis.com"
  defp gcm_port(config),  do: config[:port]     || 443

  def start_link(name, config) do
    GenServer.start_link(__MODULE__, {:ok, config}, name: name)
  end

  def stop, do: :gen_server.cast(self(), :stop)

  def init({:ok, config}) do
    Process.flag(:trap_exit, true)
    {:ok, new_state(config)}
  end

  defp new_state(config, socket \\ nil) do
    %{
      gcm_socket: socket,
      key: config[:key],
      queue: %{},
      config: config
    }
  end

  def initialize_worker(config) do
    case connect_socket(config, 0) do
      {:ok, socket} ->
        {:ok, new_state(config, socket)}
      {:closed, _socket} ->
        Logger.error """
          Socket closed unexpectedly.
          """
        {:ok, new_state(config)}
      {:error, :timeout} ->
        Logger.error """
          Failed to establish SSL connection.
          """
        {:ok, new_state(config)}
      {:error, :invalid_config} ->
        Logger.error """
          Invalid configuration.
          """
        {:stop, {:error, :invalid_config}}
    end
  end

  def connect_socket(_config, 3), do: {:error, :timeout}
  def connect_socket(config, tries) do
    uri = config |> gcm_uri()
    port = config |> gcm_port()
    do_connect_socket(config, uri, port, tries)
  end

  defp do_connect_socket(config, uri, port, tries) do
    case Pigeon.H2.open(uri, port) do
      {:ok, socket} -> {:ok, socket}
      {:error, reason} ->
        Logger.error(~s"Unable to open connection to FCM server due to #{inspect(reason)}")
        connect_socket(config, tries + 1)
    end
  end

  def handle_cast(:stop, state), do: { :noreply, state }

  def handle_cast({:push, :gcm, notification, on_response, %{gcm_key: key}}, state) do
    send_push(state, notification, on_response, key)
  end

  def handle_cast({:push, :gcm, notification, on_response, _opts}, state) do
    send_push(state, notification, on_response)
  end

  def handle_cast(msg, state) do
    Logger.debug "Recv: #{inspect(msg)}"
    {:noreply, state}
  end

  def send_push(%{gcm_socket: nil, config: config}, notification, on_reponse) do
    Logger.info "Reconnecting FCM client before request"
    case initialize_worker(config) do
      {:ok, newstate} -> send_push(newstate, notification, on_reponse)
      error -> error
    end
  end


  def send_push(%{key: key } = state, payload, on_response) do
    send_push(state, payload, on_response, key)
  end

  def send_push(%{gcm_socket: socket, queue: queue} = state,
      {registration_ids, payload}, on_response, key) do
    req_headers = [
      { "authorization", "key=#{key}" },
      { "content-type", "application/json" },
      { "accept", "application/json" },
    ]
    uri = gcm_uri(state[:config])
    case Pigeon.H2.post(socket, uri, "/fcm/send", req_headers, payload) do
      {:ok, stream_id} ->
        new_q = Map.put(queue, "#{stream_id}", {registration_ids, on_response})
        {:noreply, %{state | queue: new_q }}
      {:error, reason} ->
        unless on_response == nil do on_response.({:error, reason}) end
        {:noreply, state}
    end
  end

  defp parse_error(data) do
    {:ok, response} = Poison.decode(data)
    response["reason"] |> Macro.underscore |> String.to_existing_atom
  end

  defp log_error(code, reason) do
    Logger.error("#{reason}: #{code}")
  end

  def handle_info({:END_STREAM, stream_id}, %{gcm_socket: socket, queue: queue} = state) do
    {:ok, {headers, body}} = Pigeon.H2.receive(socket, stream_id)

    {registration_ids, on_response} = queue["#{stream_id}"]
    case get_status(headers) do
      "200" ->
        result =  Poison.decode! body
        parse_result(registration_ids, result, on_response)
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      nil ->
        {:noreply, state}
      "401" ->
        log_error("401", "Unauthorized")
        unless on_response == nil do on_response.({:error, :unauthorized}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      "400" ->
        log_error("400", "Malformed JSON")
        unless on_response == nil do on_response.({:error, :malformed_json}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
      code ->
        reason = parse_error(body)
        log_error(code, reason)
        unless on_response == nil do on_response.({:error, reason}) end
        new_queue = Map.delete(queue, "#{stream_id}")
        {:noreply, %{state | queue: new_queue}}
    end
  end

  def handle_info({:ping, _from}, state), do: {:noreply, state}

  def handle_info({:closed, _from}, state) do
    Logger.info "FCM client connection closed "
    {:noreply, %{state | gcm_socket: nil}}
  end

  def handle_info({:ok, _from}, state), do: {:noreply, state}
  def handle_info({:EXIT, socket, _}, %{gcm_socket: socket} = state) do
    {:noreply, %{state | gcm_socket: nil}}
  end
  def handle_info({:EXIT, _socket, _}, state) do
    {:noreply, state}
  end

  def handle_info(unknown, state) do
    Logger.warn(~s"Unknown info #{inspect unknown}")
    {:noreply, state}
  end

  # no on_response callback, ignore
  def parse_result(_, _, nil), do: :ok

  def parse_result(ids, %{"results" => results}, on_response) do
    parse_result1(ids, results, on_response, %NotificationResponse{})
  end

  def parse_result1([], [], on_response, result) do
    on_response.({:ok, result})
  end

  def parse_result1(regid, results, on_response, result) when is_binary(regid) do
    parse_result1([regid], results, on_response, result)
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id, "registration_id" => new_regid} | rest_results],
                    on_response,
                    %NotificationResponse{ update: update} =  resp) do

    new_updates = [{regid, new_regid} | update]
    parse_result1(reg_res, rest_results, on_response, %{resp | message_id: id, update: new_updates})
  end

  def parse_result1([regid | reg_res],
                    [%{"message_id" => id} | rest_results],
                    on_response,
                    %NotificationResponse{ok: ok} = resp) do

    parse_result1(reg_res, rest_results, on_response, %{resp | message_id: id, ok: [regid | ok]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => "Unavailable"} | rest_results],
                    on_response,
                    %NotificationResponse{retry: retry} = resp) do

    parse_result1(reg_res, rest_results, on_response, %{resp | retry: [regid | retry]})
  end

  def parse_result1([regid | reg_res],
                    [%{"error" => invalid } | rest_results],
                    on_response,
                    %NotificationResponse{remove: remove} = resp) when invalid == "NotRegistered"
                                                                    or invalid == "InvalidRegistration" do

    parse_result1(reg_res, rest_results, on_response, %{resp | remove: [regid | remove]})
  end

  def parse_result1([regid | reg_res] = regs,
                    [%{"error" => error} | rest_results] = results,
                    on_response,
                    %NotificationResponse{error: regs_in_error} = resp) do

    case Map.has_key?(regs_in_error, error) do
      true ->
        parse_result1(reg_res, rest_results, on_response,
          %{resp | error: %{regs_in_error | error => regid}})
      false -> # create map key if required.
         parse_result1(regs, results, on_response,
          %{resp | error: Map.merge(%{error => []}, regs_in_error)})
    end
  end

  defp get_status(nil), do: nil
  defp get_status(headers) do
    case Enum.find(headers, fn({key, _val}) -> key == ":status" end) do
      {":status", status} -> status
      nil -> nil
    end
  end
end
