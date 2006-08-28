(*  Turn off compiler messages.  *)

(*  Compiler.Control.Print.out := {say=fn _=>(), flush=fn()=>()};  *)

(*  Example: timeit (benchmark, 1000, 10) returns the user time in seconds  *)
(*  required to run benchmark(1000) 10 times.                               *)

fun timeit (f, n) =
  let open Time
      open Timer
      val t = startCPUTimer()
      val rt = startRealTimer()
      val z = f n
      val { usr = tu, sys = ts, gc = tg } = checkCPUTimer t
      val rt' = checkRealTimer rt
  in (print
      (concat ["Non-gc time: ", toString (tu-tg), " seconds\n",
               "GC time:     ", toString tg, " seconds\n",
               "System time: ", toString ts, " seconds\n",
               "Real time:   ", toString rt', " seconds\n"]);
      toReal tu)
  end

fun run_benchmark (s: string, n, thunk, f) =
  let fun loop 1 = if not (f (thunk()))
                      then print ("Result is incorrect\n")
                      else ()
        | loop n = (thunk(); loop (n - 1))
      val t = timeit (loop, n)
      open Real
  in (print s; print ": "; print (toString t); print " seconds\n")
  end
