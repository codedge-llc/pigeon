defmodule Pigeon.APNS.Shared do
  @moduledoc false

  import Pigeon.Tasks, only: [process_on_response: 2]

  alias Pigeon.APNS.{Config, Notification, Error}
end
