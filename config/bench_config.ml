type benchmark_ctx = unit
type arg = unit

let[@inline] around_benchmark ~f = (f [@inlined hint]) ()
let[@inline] around_measurement () ~f = (f [@inlined hint]) ()
