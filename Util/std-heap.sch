(define *files* 
  '("Auxlib/misc"
    "Auxlib/sort"
    "Auxlib/pp"
    "Experimental/apropos"
    "Experimental/system-stuff"
    "Experimental/applyhook"
    "Debugger/debug"
    "Debugger/countcalls"))

(define *mal-files*
  '("Experimental/applyhook0"))

(define (munge files ext)
  (map (lambda (f) (string-append f ext)) files))

(define (compile-files)
  (for-each compile-file (munge *files* ".sch"))
  (for-each assemble-file (munge *mal-files* ".mal")))

(define (load-files)
  (for-each load (munge (append *mal-files* *files*) ".fasl")))

(define (dump-std-heap)
  (set! *files* (undefined))
  (set! *mal-files* (undefined))
  (set! munge (undefined))
  (set! compile-files (undefined))
  (set! dump-std-heap (undefined))
  (set! load-files (undefined))
  (set! pp pretty-print)
  (dump-interactive-heap "std.heap"))

