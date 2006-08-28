(* TAKL -- The TAKeuchi function using lists as counters. *)
 
fun list_n n =
  let fun loop (n, a) =
        if n = 0
          then a
          else loop (n-1, n::a)
  in loop (n, [])
  end

val l18 = list_n 18
val l12 = list_n 12
val l6  = list_n  6

(* Part of the fun of this benchmark is seeing how well the compiler
   can understand this ridiculous code, which dates back to the original
   Common Lisp.  So it probably isn't a good idea to improve upon it. *)

fun shorterp (x, y) =
  not (y = [])
  andalso (x = [] orelse shorterp (tl x, tl y))

(* But SML/NJ runs this benchmark about 15 times as fast when the
   code above is rewritten as follows.  So I did it.  Pooh. *)

fun shorterp (_, []) = false
  | shorterp ([], _) = true
  | shorterp (_::x, _::y) = shorterp (x, y)

fun mas (x, y, z) =
  if not (shorterp (y, x))
    then z
    else mas (mas (tl x, y, z),
              mas (tl y, z, x),
              mas (tl z, x, y))

fun takl_benchmark (n) =
  run_benchmark ("takl", n, fn () => mas (l18, l12, l6),
                            fn (x) => length(x) = 7)

fun main () = takl_benchmark (takl_iters)
