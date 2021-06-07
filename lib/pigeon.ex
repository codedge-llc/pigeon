defmodule Pigeon do
  @moduledoc """
  HTTP2-compliant wrapper for sending iOS and Android push notifications.

  ## Getting Started

  Check the module documentation for your push notification service.

    - `Pigeon.ADM` - Amazon Android.
    - `Pigeon.APNS` - Apple iOS.
    - `Pigeon.FCM` - Firebase Cloud Messaging v1 API.
    - `Pigeon.LegacyFCM` - Firebase Cloud Messaging Legacy API.

  ## Creating Dynamic Runtime Dispatchers

  Pigeon can spin up dynamic dispatchers for a variety of advanced use-cases, such as
  supporting dozens of dispatcher configurations or custom connection pools.

  See `Pigeon.Dispatcher` for instructions.

  ## Writing a Custom Dispatcher Adapter

  Want to write a Pigeon adapter for an unsupported push notification service?

  See `Pigeon.Adapter` for instructions.
  """

  alias Pigeon.Tasks

  @default_timeout 5_000

  @typedoc ~S"""
  Async callback for push notifications response.

  ## Examples

      handler = fn(%Pigeon.ADM.Notification{response: response}) ->
        case response do
          :success ->
            Logger.debug "Push successful!"
          :unregistered ->
            Logger.error "Bad device token!"
          _error ->
            Logger.error "Some other error happened."
        end
      end

      n = Pigeon.ADM.Notification.new("token", %{"message" => "test"})
      Pigeon.ADM.push(n, on_response: handler)
  """
  @type on_response ::
          (notification -> no_return)
          | {module, atom}
          | {module, atom, [any]}

  @type notification :: %{__meta__: Pigeon.Metadata.t()}

  @typedoc ~S"""
  Options for sending push notifications.

  - `:on_response` - Optional async callback triggered on receipt of push.
    See `t:on_response/0`
  - `:timeout` - Push timeout. Defaults to 5000ms.
  """
  @type push_opts :: [
          on_response: on_response | nil,
          timeout: non_neg_integer
        ]

  @doc """
  Returns the configured default pool size for Pigeon dispatchers.
  To customize this value, include the following in your config/config.exs:

      config :pigeon, :default_pool_size, 5
  """
  @spec default_pool_size :: pos_integer
  def default_pool_size() do
    Application.get_env(:pigeon, :default_pool_size, 5)
  end

  @doc """
  Returns the configured JSON encoding library for Pigeon.
  To customize the JSON library, include the following in your config/config.exs:

      config :pigeon, :json_library, Jason
  """
  @spec json_library :: module
  def json_library do
    Application.get_env(:pigeon, :json_library, Jason)
  end

  @spec push(pid | atom, notification :: struct, push_opts) ::
          notification :: struct | no_return
  @spec push(pid | atom, notifications :: [struct, ...], push_opts) ::
          notifications :: [struct, ...] | no_return
  def push(pid, notifications, opts \\ [])

  def push(pid, notifications, opts) when is_list(notifications) do
    if Keyword.has_key?(opts, :on_response) do
      on_response = Keyword.get(opts, :on_response)

      notifications =
        Enum.map(notifications, fn n ->
          put_on_response(n, on_response)
        end)

      push_async(pid, notifications)
    else
      timeout = Keyword.get(opts, :timeout, @default_timeout)
      for n <- notifications, do: push_sync(pid, n, timeout), into: []
    end
  end

  def push(pid, notification, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    if Keyword.has_key?(opts, :on_response) do
      notification =
        put_on_response(notification, Keyword.get(opts, :on_response))

      push_async(pid, notification)
    else
      push_sync(pid, notification, timeout)
    end
  end

  defp push_sync(pid, notification, timeout) do
    myself = self()
    ref = :erlang.make_ref()
    on_response = fn x -> send(myself, {:"$push", ref, x}) end
    notification = put_on_response(notification, on_response)

    push_async(pid, notification)

    receive do
      {:"$push", ^ref, x} -> x
    after
      timeout -> %{notification | response: :timeout}
    end
  end

  defp push_async(pid, notifications) when is_list(notifications) do
    for n <- notifications, do: push_async(pid, n)
    :ok
  end

  defp push_async(pid, notification) do
    case Pigeon.Registry.next(pid) do
      nil ->
        Tasks.process_on_response(%{notification | response: :not_started})

      pid ->
        send_push(pid, notification)
    end
  end

  defp send_push(pid, notification) do
    send(pid, {:"$push", notification})
    :ok
  end

  defp put_on_response(notification, on_response) do
    meta = %{notification.__meta__ | on_response: on_response}
    %{notification | __meta__: meta}
  end
end
