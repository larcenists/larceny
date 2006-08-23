(******************************************************************************
* File:         diviter.sml
* Description:  DIV benchmarks
* Author:       Richard Gabriel
* Created:      8-Apr-85
* Modified:     19-Jul-85 18:28:01 (Bob Shaw)
*               23-Jul-87 (Will Clinger: translated into Scheme)
*               4-May-94 (Will Clinger: translated into SML)
*               24-Jun-99 (Will Clinger: split into two files)
* Language:     Standard ML
* Status:       Public Domain
******************************************************************************)
 
(*  DIV2 -- Benchmark which divides by 2 using lists of n []'s.              *)

fun create_n n =
  let fun loop (n, a) =
    if n = 0
      then a
      else loop (n-1, []::a)
  in loop (n, [])
  end

val ll = create_n 200

fun recursive_div2 [] = []
  | recursive_div2 (x::y::z) = x :: (recursive_div2 z)

fun div_rec (n) =
  run_benchmark ("divrec", n, fn () => recursive_div2 ll,
                              fn (x) => length x = 100)

fun main () = div_rec (diviter_iters)
