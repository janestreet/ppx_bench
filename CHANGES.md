## 113.24.00

- Update to follow `Ppx_core` evolution.

- Mark attributes as handled inside explicitly dropped pieces of code.

  So that a `@@deriving` inside a let%test dropped by
  ppx\_inline\_test\_drop doesn't cause a failure.
