(** Default Bench_config module, available ambiently everywhere [ppx_bench] is included.

    The [around_measurement] and [around_benchmark] functions in this module do nothing. *)

(** @open *)
include Bench_config_types.S with type benchmark_ctx = unit and type arg = unit
