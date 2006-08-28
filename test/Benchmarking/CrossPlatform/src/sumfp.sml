(* SUM -- Compute sum of integers from 0 to 10000 *)

fun run () =
  let fun loop (i, n) =
        if i < 0.0
           then n
           else loop (i - 1.0, i + n)
  in loop (10000.0, 0.0)
  end

fun sumfp_benchmark (n) =
  let open Real
  in run_benchmark ("sumfp", n, fn () => run(), fn (x) => ==(x, 50005000.0))
  end

fun main () = sumfp_benchmark (sumfp_iters)
