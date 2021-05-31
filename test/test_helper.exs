ExUnit.start(capture_log: true)

workers = [
  PigeonTest.ADM,
  PigeonTest.APNS,
  PigeonTest.APNS.JWT,
  PigeonTest.FCM,
  PigeonTest.LegacyFCM
]

Supervisor.start_link(workers, strategy: :one_for_one)
