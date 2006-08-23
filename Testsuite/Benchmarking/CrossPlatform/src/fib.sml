fun fib n =
  if n < 2
     then n
     else fib(n-1) + fib(n-2)

fun fib_benchmark (n) =
  run_benchmark ("fib", n, fn () => fib 25, fn (x) => x = 75025)

fun main () = fib_benchmark (fib_iters)
