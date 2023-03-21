defmodule Pigeon.Pushy.ResultParser do
  @moduledoc false
  alias Pigeon.Pushy.Error

  def parse(
        notification,
        response = %{
          "id" => push_id,
          "success" => success_status,
          "info" => %{"devices" => num_devices}
        }
      ) do
    notification =
      notification
      |> Map.put(:push_id, push_id)
      |> Map.put(:success, success_status)
      |> Map.put(:successful_device_count, num_devices)

    if match?(%{"info" => %{"failed" => _}}, response) do
      notification
      |> Map.put(:failed, response["info"]["failed"])
    else
      notification
    end
  end

  def parse(notification, response = %{"code" => _}) do
    Error.parse(notification, response)
  end
end
