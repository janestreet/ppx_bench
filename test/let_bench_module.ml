let%bench_module "a module" =
  (module struct
    let%bench "a microbenchmark" = ()
  end)
;;
