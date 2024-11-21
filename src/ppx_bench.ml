open Ppxlib
open Stdppx
open Ast_builder.Default

module List = struct
  include List

  let partition_map l ~f = partition_map f l
end

type maybe_drop =
  | Keep
  | Deadcode
  | Remove

let drop_benches = ref Keep
let allow_let_bench_module = ref false
let allow_let_bench_module_flag = "-bench-allow-let-bench-module"

let () =
  Driver.add_arg
    "-bench-drop"
    (Unit (fun () -> drop_benches := Remove))
    ~doc:" Drop inline benchmarks";
  Driver.add_arg
    "-bench-drop-with-deadcode"
    (Unit (fun () -> drop_benches := Deadcode))
    ~doc:
      " Drop inline benchmarks by wrapping them inside deadcode to prevent unused \
       variable warnings.";
  Driver.add_arg
    allow_let_bench_module_flag
    (Set allow_let_bench_module)
    ~doc:" Allow [let%bench_module]; otherwise, require newer form [module%bench]."
;;

let () =
  Driver.Cookies.add_simple_handler
    "inline-bench"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
      | None -> ()
      | Some id ->
        (match id.txt with
         | "drop" -> drop_benches := Remove
         | "drop_with_deadcode" -> drop_benches := Deadcode
         | s ->
           Location.raise_errorf
             ~loc:id.loc
             "invalid 'inline-bench' cookie (%s), expected one of: drop, \
              drop_with_deadcode"
             s))
;;

let maybe_drop loc code =
  match !drop_benches with
  | Keep -> [%str let () = [%e code]]
  | Deadcode -> [%str let () = if false then [%e code] else ()]
  | Remove ->
    Attribute.explicitly_drop#expression code;
    [%str]
;;

let descr (loc : Location.t) ?(inner_loc = loc) () =
  let filename = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  let start_pos = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let end_pos = inner_loc.Location.loc_end.pos_cnum - loc.loc_start.pos_bol in
  estring ~loc filename, eint ~loc line, eint ~loc start_pos, eint ~loc end_pos
;;

let apply_to_descr_bench
  type_conv_path
  lid
  loc
  ?inner_loc
  e_opt
  ?name_suffix
  name
  more_arg
  =
  let filename, line, start_pos, end_pos = descr loc ?inner_loc () in
  let s =
    match e_opt with
    | None -> ""
    | Some e -> Pprintast.string_of_expression e
  in
  let descr = estring ~loc s in
  let name =
    let base_name = estring ~loc name in
    match name_suffix with
    | None -> base_name
    | Some name_suffix -> [%expr [%e base_name] ^ [%e name_suffix]]
  in
  let type_conv_path = estring ~loc type_conv_path in
  maybe_drop
    loc
    [%expr
      if Ppx_bench_lib.Benchmark_accumulator.add_environment_var
      then
        [%e evar ~loc ("Ppx_bench_lib.Benchmark_accumulator." ^ lid)]
          ~name:[%e name]
          ~code:[%e descr]
          ~type_conv_path:[%e type_conv_path]
          ~filename:[%e filename]
          ~line:[%e line]
          ~startpos:[%e start_pos]
          ~endpos:[%e end_pos]
          [%e more_arg]]
;;

type bench_kind =
  | Bench
  | Bench_fun

type arg_kind =
  | Indexed of (string * expression)
  | Parameterised of (string * expression)

let thunk_bench kind e =
  match kind with
  | Bench_fun -> e
  | Bench ->
    let loc = { e.pexp_loc with loc_ghost = true } in
    [%expr fun () -> [%e e]]
;;

let enabled () =
  match Ppx_inline_test_libname.get () with
  | None -> false
  | Some _ -> true
;;

let assert_enabled loc =
  if not (enabled ())
  then
    Location.raise_errorf
      ~loc
      "ppx_bench: extension is disabled as no -inline-test-lib was given"
;;

let expand_bench_exp ~loc ~path kind index name e =
  let loc = { loc with loc_ghost = true } in
  assert_enabled loc;
  match index with
  | None ->
    (* Here and in the other cases below, because functions given to pa_bench can return
       any 'a, we add a dead call to ignore so we can get a warning if the user code
       mistakenly gives a partial application. *)
    apply_to_descr_bench
      path
      "add_bench"
      loc
      (Some e)
      name
      [%expr
        let f `init =
          { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried = [%e thunk_bench kind e]
          }
        in
        if false then Ppx_bench_lib.Export.ignore ((f `init).uncurried ()) else ();
        Ppx_bench_lib.Benchmark_accumulator.Entry.Regular_thunk f]
  | Some (Indexed (var_name, args)) ->
    apply_to_descr_bench
      path
      "add_bench"
      loc
      (Some e)
      name
      [%expr
        let arg_values = [%e args]
        and f [%p pvar ~loc var_name] =
          { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried = [%e thunk_bench kind e]
          }
        in
        if false then Ppx_bench_lib.Export.ignore ((f 0).uncurried ()) else ();
        Ppx_bench_lib.Benchmark_accumulator.Entry.Parameterised_thunk
          { Ppx_bench_lib.Benchmark_accumulator.Entry.arg_name =
              [%e estring ~loc var_name]
          ; Ppx_bench_lib.Benchmark_accumulator.Entry.params =
              (* We use Stdlib.* because this might run without any opens. *)
              Stdlib.List.map
                (fun i -> Stdlib.string_of_int i, i)
                arg_values [@warning "-3"]
          ; Ppx_bench_lib.Benchmark_accumulator.Entry.thunk = f
          }]
  | Some (Parameterised (var_name, args)) ->
    apply_to_descr_bench
      path
      "add_bench"
      loc
      (Some e)
      name
      [%expr
        let params = [%e args]
        and f [%p pvar ~loc var_name] =
          { Ppx_bench_lib.Benchmark_accumulator.Entry.uncurried = [%e thunk_bench kind e]
          }
        in
        if false
        then Ppx_bench_lib.Export.ignore ((f (List.hd_exn params |> snd)).uncurried ())
        else ();
        Ppx_bench_lib.Benchmark_accumulator.Entry.Parameterised_thunk
          { Ppx_bench_lib.Benchmark_accumulator.Entry.arg_name =
              [%e estring ~loc var_name]
          ; Ppx_bench_lib.Benchmark_accumulator.Entry.params
          ; Ppx_bench_lib.Benchmark_accumulator.Entry.thunk = f
          }]
;;

let expand_bench_module ~is_let_bench_module ~loc ~path name_suffix name m =
  let loc = { loc with loc_ghost = true } in
  if is_let_bench_module && not !allow_let_bench_module
  then
    Location.raise_errorf
      ~loc
      "Convert [%s] to [%s] or pass [%s] to ppx driver"
      "let%bench_module"
      "module%bench"
      allow_let_bench_module_flag;
  assert_enabled loc;
  apply_to_descr_bench
    path
    "add_bench_module"
    loc
    ~inner_loc:m.pmod_loc
    None
    ?name_suffix
    name
    (pexp_fun
       ~loc
       Nolabel
       None
       (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc (Some "M")) m (eunit ~loc)))
;;

module E = struct
  let indexed =
    Attribute.declare
      "bench.indexed"
      Attribute.Context.pattern
      Ast_pattern.(
        single_expr_payload
          (pexp_apply
             (pexp_ident (lident (string "=")))
             (no_label (pexp_ident (lident __)) ^:: no_label __ ^:: nil)))
      (fun var values -> Indexed (var, values))
  ;;

  let parameterised =
    Attribute.declare
      "bench.params"
      Attribute.Context.pattern
      Ast_pattern.(
        single_expr_payload
          (pexp_apply
             (pexp_ident (lident (string "=")))
             (no_label (pexp_ident (lident __)) ^:: no_label __ ^:: nil)))
      (fun var values -> Parameterised (var, values))
  ;;

  let name_suffix =
    Attribute.declare
      "bench.name_suffix"
      Attribute.Context.pattern
      Ast_pattern.(single_expr_payload __)
      (fun a -> a)
  ;;

  let module_name_suffix =
    Attribute.declare
      "bench.name_suffix"
      Attribute.Context.module_binding
      Ast_pattern.(single_expr_payload __)
      (fun a -> a)
  ;;

  let module_name_pattern pat =
    Ast_pattern.of_func (fun ctx loc mb k ->
      let name_attrs, other_attrs =
        List.partition_map mb.pmb_attributes ~f:(fun attr ->
          match attr with
          | { attr_name = { txt = "name"; loc = _ }
            ; attr_payload =
                PStr
                  [%str
                    [%e? { pexp_desc = Pexp_constant (Pconst_string (name, _, _)); _ }]]
            ; attr_loc = _
            } -> Left (attr, name)
          | _ -> Right attr)
      in
      match name_attrs with
      | [] -> Ast_pattern.to_func pat ctx loc mb (k None)
      | [ (attr, name) ] ->
        Attribute.mark_as_handled_manually attr;
        Ast_pattern.to_func
          pat
          ctx
          loc
          { mb with pmb_attributes = other_attrs }
          (k (Some name))
      | _ :: _ :: _ -> Location.raise_errorf ~loc "duplicate @name attribute")
  ;;

  let simple =
    let open Ast_pattern in
    pstr
      (pstr_value
         nonrecursive
         (value_binding
            ~pat:
              (alt
                 (Attribute.pattern indexed (pstring __))
                 (Attribute.pattern parameterised (pstring __)))
            ~expr:__
          ^:: nil)
       ^:: nil)
  ;;

  let module_ =
    let open Ast_pattern in
    pstr
      (pstr_module
         (module_binding ~name:__ ~expr:__
          |> module_name_pattern
          |> Attribute.pattern module_name_suffix
          |> map0' ~f:(fun x -> x)
          |> map ~f:(fun f loc name_suffix attr_name bind_name m ->
            let name =
              match attr_name, bind_name with
              | None, None -> ""
              | Some name, None | None, Some name -> name
              | Some attr_name, Some bind_name ->
                Location.raise_errorf
                  ~loc
                  "multiple names; use one of:\n\
                  \  [module%%bench %s =], or\n\
                  \  [module%%bench [@name %S] _ =],\n\
                   but not both."
                  bind_name
                  attr_name
            in
            f name_suffix name m))
       ^:: nil)
  ;;

  let simple_or_module =
    let open Ast_pattern in
    map simple ~f:(fun f index name e -> f (`Bench (index, name, e)))
    ||| map module_ ~f:(fun f suffix name m -> f (`Module (suffix, name, m)))
  ;;

  let bench =
    Extension.declare_inline
      "bench"
      Extension.Context.structure_item
      simple_or_module
      (fun ~loc ~path -> function
      | `Bench (index, name, e) -> expand_bench_exp ~loc ~path Bench index name e
      | `Module (suffix, name, m) ->
        expand_bench_module ~is_let_bench_module:false ~loc ~path suffix name m)
  ;;

  let bench_fun =
    Extension.declare_inline
      "bench_fun"
      Extension.Context.structure_item
      simple
      (expand_bench_exp Bench_fun)
  ;;

  let bench_module =
    Extension.declare_inline
      "bench_module"
      Extension.Context.structure_item
      Ast_pattern.(
        pstr
          (pstr_value
             nonrecursive
             (value_binding
                ~pat:(Attribute.pattern name_suffix (pstring __))
                ~expr:(pexp_pack __)
              ^:: nil)
           ^:: nil))
      (expand_bench_module ~is_let_bench_module:true)
  ;;

  let all = [ bench; bench_fun; bench_module ]
end

let () =
  Driver.register_transformation "bench" ~extensions:E.all ~enclose_impl:(fun loc ->
    match loc, Ppx_inline_test_libname.get () with
    | None, _ | _, None -> [], []
    | Some loc, Some (libname, _) ->
      let loc = { loc with loc_ghost = true } in
      (* See comment in benchmark_accumulator.ml *)
      let header =
        let loc = { loc with loc_end = loc.loc_start } in
        maybe_drop
          loc
          [%expr
            Ppx_bench_lib.Benchmark_accumulator.Current_libname.set
              [%e estring ~loc libname]]
      and footer =
        let loc = { loc with loc_start = loc.loc_end } in
        maybe_drop
          loc
          [%expr Ppx_bench_lib.Benchmark_accumulator.Current_libname.unset ()]
      in
      header, footer)
;;
