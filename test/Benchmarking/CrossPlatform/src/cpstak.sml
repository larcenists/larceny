fun cpstak (x, y, z) =
  let fun tak (x, y, z, k) =
        if not (y < x)
          then k z
          else tak (x - 1,
                    y,
                    z,
                    fn (v1) =>
                      tak (y - 1,
                           z,
                           x,
                           fn (v2) =>
                             tak (z - 1,
                                  x,
                                  y,
                                  fn (v3) =>
                                    tak (v1, v2, v3, k))))
  in tak (x, y, z, fn (a) => a)
  end

fun cpstak_benchmark (n) =
  run_benchmark ("cpstak", n, fn () => cpstak (18, 12, 6),
                              fn (x) => x = 7)

fun main () = cpstak_benchmark (cpstak_iters)

