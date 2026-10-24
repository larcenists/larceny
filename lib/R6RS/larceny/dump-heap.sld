; (larceny dump-heap): scheme-dump-heap and scheme-dump-interactive-heap
; for R7RS and R6RS programs; see lib/Experimental/scheme-dump-heap.sch.

(define-library (larceny dump-heap)
  (export scheme-dump-heap scheme-dump-interactive-heap)
  (import (scheme base)
          (scheme cxr)
          (scheme file)
          (scheme write)
          (primitives fxlogand fxlogior fxlogxor fxlsh fxrshl
                      .internal:machine-address gc-counter
                      procedure-ref procedure-length
                      vector-like? vector-like-length
                      bytevector-like? bytevector-like-length
                      sro syscall system-features reset-all-hashtables!
                      call-without-interrupts command-line-arguments
                      dump-heap dump-interactive-heap
                      bytevector-ref bytevector-set!
                      open-file-output-port put-bytevector close-port))
  (include "../../Experimental/scheme-dump-heap.sch"))
