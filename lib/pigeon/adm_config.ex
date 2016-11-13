defmodule Pigeon.ADM.Config do
  @moduledoc """
    Validates configuration settings that initialize ADMWorkers.
  """

  defp config_adm_client_id, do: Application.get_env(:pigeon, :adm)[:client_id]
  defp config_adm_client_secret, do: Application.get_env(:pigeon, :adm)[:client_secret]

  def default_config do
    %{
      client_id: config_adm_client_id,
      client_secret: config_adm_client_secret
    }
  end

  def configured? do
    client_id = Application.get_env(:pigeon, :adm)[:client_id]
    client_secret = Application.get_env(:pigeon, :adm)[:client_secret]
    !is_nil(client_id) and !is_nil(client_secret)
  end

  def valid?(config) do
    client_id = config[:client_id]
    valid_client_id? = is_binary(client_id) and String.length(client_id) > 0

    client_secret = config[:client_secret]
    valid_client_secret? = is_binary(client_secret) and String.length(client_secret) > 0

    valid_client_id? and valid_client_secret?
  end
end
