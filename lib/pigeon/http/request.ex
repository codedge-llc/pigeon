defmodule Pigeon.HTTP.Request do
  @moduledoc false

  defstruct body: "",
            done?: false,
            headers: [],
            error: nil,
            notification: nil,
            status: nil

  @type t :: %__MODULE__{
          body: String.t(),
          done?: boolean(),
          headers: [{String.t(), String.t()}],
          error: any(),
          notification: any(),
          status: non_neg_integer() | nil
        }

  def new(notification) do
    %__MODULE__{notification: notification}
  end

  def merge(request, params) do
    body = request.body <> (params[:body] || "")
    headers = request.headers ++ (params[:headers] || [])
    error = request.error || params[:error]
    status = request.status || params[:status]
    done? = request.done? || params[:done?] || false

    %{
      request
      | body: body,
        done?: done?,
        error: error,
        headers: headers,
        status: status
    }
  end
end
