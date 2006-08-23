(* SUM -- Compute sum of integers from 0 to 10000 *)

fun run () =
  let fun loop (i, n) =
        if i < 0
           then n
           else loop (i - 1, i + n)
  in loop (10000, 0)
  end

fun sum_benchmark (n) =
  run_benchmark ("sum", n, fn () => run(), fn (x) => x = 50005000)

fun main () = sum_benchmark (sum_iters)
