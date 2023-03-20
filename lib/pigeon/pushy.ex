defmodule Pigeon.Pushy do
  @moduledoc """
  `Pigeon.Adapter` for Pushy pushy notifications
  """
  import Pigeon.Tasks, only: [process_on_response: 1]
  require Logger

  alias Pigeon.Pushy.Error

  defstruct config: nil

  @behaviour Pigeon.Adapter

  @impl true
  def init(opts) do
    config = Pigeon.Pushy.Config.new(opts)
    Configurable.validate!(config)

    state = %__MODULE__{config: config}

    {:ok, state}
  end

  @impl true
  def handle_push(notification, state) do
    :ok = do_push(notification, state)
    {:noreply, state}
  end

  @impl true
  def handle_info({_from, {:ok, %HTTPoison.Response{status_code: 200}}}, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp do_push(notification, state) do
    encoded_notification = encode_payload(notification)

    response = fn notification ->
      case HTTPoison.post(pushy_uri(state.config), notification, pushy_headers()) do
        {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
          process_response(status, body, notification)

        {:error, %HTTPoison.Error{reason: :connect_timeout}} ->
          notification
          |> Map.put(:response, :timeout)
          |> process_on_response()
      end
    end

    Task.Supervisor.start_child(Pigeon.Tasks, fn -> response.(encoded_notification) end)
    :ok
  end

  defp pushy_uri(%Pigeon.Pushy.Config{uri: base_uri, key: secret_key}) do
    "https://#{base_uri}/pushy/?api_key=#{secret_key}"
  end

  def pushy_headers() do
    [
      {"Content-Type", "application/json"},
      {"Accept", "application/json"}
    ]
  end

  defp process_response(200, body, notification),
    do: handle_200_status(body, notification)

  defp process_response(status, body, notification),
    do: handle_error_status_code(status, body, notification)

  defp handle_200_status(body, notification) do
    {:ok, json} = Pigeon.json_library().decode(body)

    notification
    |> Error.parse(json)
    |> process_on_response()
  end

  defp handle_error_status_code(status, body, notification) do
    case Pigeon.json_library().decode(body) do
      {:ok, %{"reason" => _reason} = result_json} ->
        notification
        |> Error.parse(result_json)
        |> process_on_response()

      {:error, _} ->
        notification
        |> Map.put(:response, generic_error_reason(status))
        |> process_on_response()
    end
  end

  defp generic_error_reason(400), do: :invalid_json
  defp generic_error_reason(401), do: :authentication_error
  defp generic_error_reason(500), do: :internal_server_error
  defp generic_error_reason(_), do: :unknown_error
end
