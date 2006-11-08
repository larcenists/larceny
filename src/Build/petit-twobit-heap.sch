; Load script for building a heap image with all compiler names exposed.
; For Petit Larceny only.  Make sure you start twobit.bin with -stopcopy.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

;(display "Thanks for dumping a Petit Twobit heap.  Now you need to say\n")
;(display "something like (dump) or (dump 'nasm)\n")
;(display "(N.B.: heap dumping only works with -stopcopy\n")

(define (dump . native)
  (load "setup.sch")

  (apply setup native)

  (load-compiler)

  (require 'petit-compile-file.sch)

  (require 'debug)
  (require 'inspect-cont)
  (require 'trace)
  (install-debugger)

  ; This feels dirty, but . . .
  (set! dump (undefined))

  (dump-interactive-heap "twobit.heap"))

(dump)

; eof
