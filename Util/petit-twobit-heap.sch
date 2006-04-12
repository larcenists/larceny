; Load script for building a heap image with all compiler names exposed.
; For Petit Larceny only.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(display "Thanks for dumping a Petit Twobit heap.  Now you need to say\n")
(display "something like (dump 'linux-el) or (dump 'win32 'native)\n")

(define (dump host . native)
  (load "Util/petit-setup.sch")
  (apply setup 'scheme: 'larceny 'host: host native)

  (load-compiler)

  (require 'debug)
  (require 'inspect-cont)
  (require 'trace)
  (install-debugger)

  ; This feels dirty, but . . .
  (set! dump (undefined))

  (dump-interactive-heap "twobit.heap"))

; eof
