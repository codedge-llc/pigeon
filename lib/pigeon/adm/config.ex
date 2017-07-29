defmodule Pigeon.ADM.Config do
  @moduledoc """
  Validates configuration settings that initialize ADM.Worker instances.
  """

  def default_name, do: :adm_default

  def config(name) do
    config = Application.get_env(:pigeon, :adm)[name]
    %{
      name: name,
      client_id: config[:client_id],
      client_secret: config[:client_secret]
    }
  end

  def valid?(config) do
    client_id = config[:client_id]
    valid_client_id? = is_binary(client_id) and String.length(client_id) > 0

    client_secret = config[:client_secret]
    valid_client_secret? = is_binary(client_secret) and String.length(client_secret) > 0

    valid_client_id? and valid_client_secret?
  end
end
