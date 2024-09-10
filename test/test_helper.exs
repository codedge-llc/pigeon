ExUnit.start(capture_log: true)

fcm_credentials =
  System.fetch_env!("FCM_SERVICE_ACCOUNT_JSON")
  |> Jason.decode!()

workers = [
  {Goth,
   name: PigeonTest.Goth, source: {:service_account, fcm_credentials, []}},
  PigeonTest.ADM,
  PigeonTest.APNS,
  PigeonTest.APNS.JWT,
  PigeonTest.FCM,
  PigeonTest.Sandbox
]

Supervisor.start_link(workers, strategy: :one_for_one)
