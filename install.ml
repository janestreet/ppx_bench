#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_bench"
  [ oasis_lib "ppx_bench"
  ; oasis_lib "ppx_bench_lib"
  ; file "META" ~section:"lib"
  ; oasis_exe "ppx" ~dest:"../lib/ppx_bench/ppx"
  ]
