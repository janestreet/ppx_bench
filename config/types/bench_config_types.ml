(** Configuration for running benchmarks *)
module type S = sig
  (** Type of the per-benchmark context, which is initialized by [around_benchmark] and
      provided to [around_measurement]. The value can also be bound in the scope of a
      [let%bench] or [let%bench_fun] using the [[@ctx?]] attribute, like so:

      {[
        let%bench_fun ("my benchmark" [@ctx? ctx]) =
          fun arg -> do_something_expensive ctx arg
        ;;
      ]} *)
  type benchmark_ctx

  (** Argument type which is passed locally to functions on the right-hand-side of
      [let%bench_fun]. If this is anything other than [unit], [let%bench] cannot be used. *)
  type arg

  (** Function which is run once around each benchmark (defined by [let%bench] or
      [let%bench_fun]) and can perform per-benchmark setup and teardown, and also
      initialize the [benchmark_ctx] argument to pass to the [around_measurement] function
      and (optionally) the benchmark itself. *)
  val around_benchmark : f:(benchmark_ctx @ local -> 'r) @ local once -> 'r

  (** Function which is invoked around each "batch" of benchmark runs, and provides [arg]
      to the benchmark itself to run. This function can be used to perform per-batch setup
      and teardown, and also provide a local context value (such as a capability) to the
      benchmark. *)
  val around_measurement
    :  benchmark_ctx @ local
    -> f:(arg @ local -> 'r) @ local once
    -> 'r
end
