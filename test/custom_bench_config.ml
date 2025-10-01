open! Base

module%bench Alternate_bench_config = struct
  module Bench_config :
    Bench_config_types.S
    with type benchmark_ctx = string array
     and type arg = string global = struct
    type benchmark_ctx = string array
    type arg = string global

    let around_benchmark ~f = f [| "foo" |]
    let around_measurement ctx ~f = f { global = ctx.(0) }
  end

  let%bench_fun "takes a string" = fun { global = s } -> s ^ "bar"

  let%bench_fun ("indexed" [@indexed suffix = [ 1; 2; 3 ]]) =
    fun { global = s } -> s ^ Int.to_string suffix
  ;;

  let%bench_fun ("parameterized"
    [@params suffix = [ "bar", "bar"; "baz", "baz"; "qux", "qux" ]])
    =
    fun { global = s } -> s ^ suffix
  ;;

  let%bench_fun ("binds the context to a var" [@context? benchmark_ctx]) =
    fun { global = s } -> s ^ benchmark_ctx.(0)
  ;;

  let%bench_fun ("indexed, and binds the context to a var"
    [@indexed idx = [ 1; 2; 3 ]] [@context? benchmark_ctx])
    =
    fun { global = s } -> s ^ benchmark_ctx.(idx)
  ;;

  let%bench_fun ("binds the context to a var, and indexed"
    [@context? benchmark_ctx] [@indexed idx = [ 1; 2; 3 ]])
    =
    fun { global = s } -> s ^ benchmark_ctx.(idx)
  ;;

  let%bench_fun ("parameterized, and binds the context to a var"
    [@params suffix = [ "bar", "bar"; "baz", "baz"; "qux", "qux" ]]
    [@context? benchmark_ctx])
    =
    fun { global = s } -> s ^ benchmark_ctx.(0) ^ suffix
  ;;

  let%bench_fun ("binds the context to a var, and parameterized"
    [@context? benchmark_ctx]
    [@params suffix = [ "bar", "bar"; "baz", "baz"; "qux", "qux" ]])
    =
    fun { global = s } -> s ^ benchmark_ctx.(0) ^ suffix
  ;;
end

module%bench Only_bench_ctx = struct
  module Bench_config :
    Bench_config_types.S
    with type benchmark_ctx = string Queue.t global
     and type arg = unit = struct
    type benchmark_ctx = string Queue.t global
    type arg = unit

    let around_benchmark ~f = f { global = Queue.create () }

    let around_measurement { global = ctx } ~f =
      let res = f () in
      Queue.clear ctx;
      res
    ;;
  end

  let%bench_fun "doesn't bind ctx" = fun () -> "foo"

  let%bench_fun ("binds ctx" [@context? { global = queue }]) =
    fun () -> Queue.enqueue_front queue "foo"
  ;;

  let%bench "non-fun, doesn't bind ctx" = "foo"

  let%bench ("non-fun, binds ctx" [@context? { global = queue }]) =
    Queue.enqueue_front queue "foo"
  ;;
end
