open! Base

[@@@disable_unused_warnings]

(* We have to define this in a separate module to see the error, so that mode variables
   have been zapped *)
module M : sig
  val f : unit -> unit
end = struct
  let f () = ()
end

[@@@expand_inline let%bench_fun "a" = fun () -> M.f ()]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"a"
      ~code:"fun () -> M.f ()"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:13
      ~startpos:18
      ~endpos:54
      ~config:(module Bench_config)
      (let f ctx__001_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun () = M.f () in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]
[@@@expand_inline let%bench_fun "a" = M.f]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"a"
      ~code:"M.f"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:45
      ~startpos:18
      ~endpos:41
      ~config:(module Bench_config)
      (let f ctx__003_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun = M.f in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]

[@@@expand_inline
  let%bench_fun "a" =
    let _some_setup_stuff = "foo" in
    let _other_setup_stuff = "bar" in
    fun () -> M.f ()
  ;;]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"a"
      ~code:
        "let _some_setup_stuff = \"foo\" in\n\
         let _other_setup_stuff = \"bar\" in fun () -> M.f ()"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:79
      ~startpos:2
      ~endpos:117
      ~config:(module Bench_config)
      (let f ctx__005_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun =
                let _some_setup_stuff = "foo" in
                let _other_setup_stuff = "bar" in
                fun () -> M.f ()
              in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]

[@@@expand_inline
  let%bench_fun "a" =
    let _some_setup_stuff = "foo" in
    let _other_setup_stuff = "bar" in
    M.f
  ;;]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"a"
      ~code:"let _some_setup_stuff = \"foo\" in let _other_setup_stuff = \"bar\" in M.f"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:123
      ~startpos:2
      ~endpos:104
      ~config:(module Bench_config)
      (let f ctx__007_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun =
                let _some_setup_stuff = "foo" in
                let _other_setup_stuff = "bar" in
                M.f
              in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]

[@@@expand_inline
  let%bench_fun "full example with lots of stuff" =
    let _some_setup_stuff = "foo" in
    let open Bench_config in
    let exception Oh_no in
    let module Foo = Int in
    ignore (1 + 1 : int);
    if 1 = 2
    then (
      match [ "foo" ] with
      | [ _ ] ->
        (try M.f with
         | Oh_no -> M.f)
      | _ -> fun () -> M.f ())
    else M.f
  ;;]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"full example with lots of stuff"
      ~code:
        "let _some_setup_stuff = \"foo\" in\n\
         let open Bench_config in\n\
        \  let exception Oh_no  in\n\
        \    let module Foo = Int in\n\
        \      ignore (1 + 1 : int);\n\
        \      if 1 = 2\n\
        \      then\n\
        \        (match [\"foo\"] with\n\
        \         | _::[] -> (try M.f with | Oh_no -> M.f)\n\
        \         | _ -> (fun () -> M.f ()))\n\
        \      else M.f"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:165
      ~startpos:2
      ~endpos:357
      ~config:(module Bench_config)
      (let f ctx__009_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun =
                let _some_setup_stuff = "foo" in
                let open Bench_config in
                let exception Oh_no in
                let module Foo = Int in
                ignore (1 + 1 : int);
                if 1 = 2
                then (
                  match [ "foo" ] with
                  | _ :: [] ->
                    (try M.f with
                     | Oh_no -> M.f)
                  | _ -> fun () -> M.f ())
                else M.f
              in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]

(* Demonstrate why we need [@warning "-ignored-extra-argument"] *)
[@@@expand_inline let%bench_fun "foo" = if true then M.f else assert false]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"foo"
      ~code:"if true then M.f else assert false"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:238
      ~startpos:18
      ~endpos:74
      ~config:(module Bench_config)
      (let f ctx__011_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun = if true then M.f else assert false in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]

(* Demonstrate why we need to type-annotate the argument *)
[@@@expand_inline let%bench_fun "const" = Fn.const ()]

let () =
  if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
  then
    Ppx_bench_lib.Benchmark_accumulator.add_bench
      ~name:"const"
      ~code:"Fn.const ()"
      ~type_conv_path:"coersion.ml"
      ~filename:"coersion.ml"
      ~line:272
      ~startpos:18
      ~endpos:53
      ~config:(module Bench_config)
      (let f ctx__013_ =
         { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried =
             (let bench_fun = Fn.const () in
              (bench_fun :> Bench_config.arg -> _))
         }
       in
       if false
       then
         Ppx_bench_lib.Export.ignore
           (Bench_config.around_benchmark ~f:(fun ctx ->
              Bench_config.around_measurement ctx ~f:(fun arg ->
                (f ctx).uncurried arg [@nontail])
              [@nontail]))
       else ();
       Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f)
;;

[@@@end]
