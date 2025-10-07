(** The point of [Benchmark_accumulator] is to provide a global place where inline
    benchmarking macros can register themselves. Once registered here, the benchmarks are
    retrieved and analyzed using [Core_bench].

    This module holds the registered benchmarks in a global hashtable indexed by library
    name. We care about the registered benchmarks if and only if the library is being used
    in a [inline_benchmarks_runner.exe]. To avoid building this hashtable in cases where
    we will not use it, this module peeks into the commandline args of the running program
    to decide if the benchmarks should be registered or not. *)
open! Stdppx

module Current_libname : sig
  val set : string -> unit
  val unset : unit -> unit
end

module Entry : sig
  (** This type exists to prevent "staged" functions with no setup from being curried. *)
  type ('arg, 'r) thunk = { uncurried : 'arg @ local -> 'r } [@@unboxed]

  type ('param, 'benchmark_ctx, 'arg, 'r) parameterised_spec =
    { arg_name : string
    ; params : (string * 'param) list
    (** The first coordinate is some string representation of the second coordinate. *)
    ; thunk : 'param -> 'benchmark_ctx @ local -> ('arg, 'r) thunk @ local
    }

  type ('benchmark_ctx, 'arg) test_spec =
    | Regular_thunk :
        ('benchmark_ctx @ local -> ('arg, 'r) thunk @ local)
        -> ('benchmark_ctx, 'arg) test_spec
    | Parameterised_thunk :
        ('param, 'benchmark_ctx, 'arg, 'r) parameterised_spec
        -> ('benchmark_ctx, 'arg) test_spec

  type ('benchmark_ctx, 'arg) t = private
    { unique_id : int
    ; code : string
    ; type_conv_path : string
    ; name : string
    ; filename : string
    ; line : int
    ; startpos : int
    ; endpos : int
    ; test_spec : ('benchmark_ctx, 'arg) test_spec
    ; config :
        (module Bench_config_types.S
           with type arg = 'arg
            and type benchmark_ctx = 'benchmark_ctx)
    ; bench_module_name : string option
    }

  type packed = P : ('benchmark_ctx, 'arg) t -> packed [@@unboxed]

  val with_test_spec
    :  ('benchmark_ctx, 'arg) t
    -> ('benchmark_ctx, 'arg) test_spec
    -> ('benchmark_ctx, 'arg) t

  val compare : _ t -> _ t -> int
  val get_module_name_opt : _ t -> string option
end

(** [add_environment_var] returns true if the benchmarks should be added to the hashtable *)
val add_environment_var : bool

(** [lookup_lib] returns all the benchmarks from the specified library *)
val lookup_lib : libname:string -> Entry.packed list

(** [add_bench] registers benchmarks with the global hashtable maintained in
    [ppx_bench_lib]. This is meant to be called by the code generated for the BENCH and
    BENCH_INDEXED macros *)
val add_bench
  :  name:string
  -> code:string
  -> filename:string
  -> type_conv_path:string
  -> line:int
  -> startpos:int
  -> endpos:int
  -> config:
       (module Bench_config_types.S
          with type arg = 'arg
           and type benchmark_ctx = 'benchmark_ctx)
  -> ('benchmark_ctx, 'arg) Entry.test_spec
  -> unit

(** [add_bench_module] adds a bench module name to the benchmarks. This is called by
    BENCH_MODULE macro *)
val add_bench_module
  :  name:string
  -> code:string
  -> type_conv_path:string
  -> filename:string
  -> line:int
  -> startpos:int
  -> endpos:int
  -> config:(module Bench_config_types.S)
  -> (unit -> unit)
  -> unit
