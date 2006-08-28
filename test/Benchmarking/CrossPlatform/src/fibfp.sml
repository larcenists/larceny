fun fibfp n =
  if n < 2.0
     then n
     else fibfp(n-1.0) + fibfp(n-2.0)

fun fibfp_benchmark (n) =
  let open Real
  in
    run_benchmark ("fibfp", n, fn () => fibfp 30.0, fn (x) => ==(x, 832040.0))
  end

fun main () = fibfp_benchmark (fibfp_iters)
