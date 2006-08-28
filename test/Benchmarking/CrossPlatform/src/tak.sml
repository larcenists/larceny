(* TAK -- A vanilla version of the TAKeuchi function. *)
 
fun tak (x, y, z) =
  if not (y < x)
    then z
    else tak (tak (x-1, y, z),
              tak (y-1, z, x),
              tak (z-1, x, y))

fun tak_benchmark (n) =
  run_benchmark ("tak", n, fn () => tak (18, 12, 6), fn (x) => x = 7)

fun main () = tak_benchmark (tak_iters)
