; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Various experiments for determining reasonable time slices.

(define (nothing)
  #t)

(define (something)
  (string-ref "abcdef" 3))

(define something-else
  (let ((x (make-string 5))
	(y "abcde"))
    (lambda ()
      (string-set! x 0 (string-ref y 4))
      (string-set! x 1 (string-ref y 3))
      (string-set! x 2 (string-ref y 2))
      (string-set! x 3 (string-ref y 1))
      (string-set! x 4 (string-ref y 0))
      x)))

(define (compute-timeslices)
  (run-benchmark "50,000 do-nothing procedure calls" nothing 50000)
  (run-benchmark "100,000 do-nothing procedure calls" nothing 100000)
  (run-benchmark "50,000 calls and string-refs" something 50000)
  (run-benchmark "100,000 calls and string-refs" something 100000)
  (run-benchmark "50,000 calls to string-manipulator" something-else 50000)
  (run-benchmark "100,000 calls to string-manipulator" something-else 100000)
  #t)
