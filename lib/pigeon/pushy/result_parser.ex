defmodule Pigeon.Pushy.ResultParser do
  @moduledoc false
  alias Pigeon.Pushy.Error

  def parse(notification, %{"id" => push_id, "success" => success_status, "info" => %{"devices" => num_devices, "failed" => failed_devices}}) do
    notification
    |> Map.put(:push_id, push_id)
    |> Map.put(:success, success_status)
    |> Map.put(:successful_device_count, num_devices)
    |> Map.put(:failed, failed_devices)
  end

  def parse(_notification, response = %{"code" => _}) do
    Error.parse(response)
  end

end
