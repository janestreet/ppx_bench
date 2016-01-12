let package_name = "ppx_bench"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_bench", None)
    ; ("built_lib_ppx_bench_lib", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
