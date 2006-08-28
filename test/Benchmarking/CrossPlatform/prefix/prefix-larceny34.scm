(define old-run-benchmark run-benchmark)

(define (run-benchmark name count run ok?)
  (old-run-benchmark
   name
   (lambda ()
     (if (not (ok? (run-bench name count run ok?)))
       (begin
         (display "*** wrong result ***")
         (newline)))))
  (exit 0))

(define (run-bench name count run ok?)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define fatal-error error)

(define FLOATabs abs)
(define FLOATsin sin)
(define FLOATcos cos)
(define FLOATatan atan)
(define FLOATsqrt sqrt)
(define FLOATmin min)
(define FLOATmax max)
(define FLOATround round)
(define FLOATinexact->exact inexact->exact)

(define bitwise-or logior)
(define bitwise-and logand)
(define bitwise-not lognot)
