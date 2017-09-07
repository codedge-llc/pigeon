defmodule Pigeon.ADM.ResultParser do
  @moduledoc false

  # Handle RegID updates
  def parse(notification, %{"registrationID" => new_regid}, on_response) do
    n = %{notification | response: :update, updated_registration_id: new_regid}
    on_response.(n)
  end

  def parse(notification, %{"reason" => error}, on_response) do
    error = error |> Macro.underscore |> String.to_existing_atom
    n = %{notification | response: error}
    on_response.(n)
  end

  def parse(notification, %{}, on_response) do
    n = %{notification | response: :success}
    on_response.(n)
  end
end
