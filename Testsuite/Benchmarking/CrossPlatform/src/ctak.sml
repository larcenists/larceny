(* CTAK -- A version of the TAK procedure that uses continuations. *)

fun ctak_aux (k, x, y, z) =
  let open SMLofNJ.Cont
  in
    if not (y < x)
      then throw k z
      else callcc
             (fn (k) =>
                ctak_aux (k,
                          callcc (fn (k) => ctak_aux (k, x-1, y, z)),
                          callcc (fn (k) => ctak_aux (k, y-1, z, x)),
                          callcc (fn (k) => ctak_aux (k, z-1, x, y))))
  end

fun ctak (x, y, z) =
  let open SMLofNJ.Cont
  in callcc (fn (k) => ctak_aux (k, x, y, z))
  end

fun ctak_benchmark (n) =
  run_benchmark ("ctak", ctak_iters, fn () => ctak (18, 12, 6),
                                     fn (x) => x = 7)

fun main () = ctak_benchmark (ctak_iters)
