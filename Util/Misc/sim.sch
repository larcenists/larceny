; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Simulates the cost of the cache flush code with various unrolling 
; strategies.

; eg.
; (sim "gclog.2" (unrolled-loop 20))	 ; cycle count for unroll 20
; (simrange "gclog.2" 10 50)             ; minimum for range 

(define (simrange fn lo hi)
  (if (<= lo hi)
      (let loop ((lo (+ lo 1))
		 (locycles (sim fn (unrolled-loop lo)))
		 (lofactor lo))
	(if (<= lo hi)
	    (let ((c (sim fn (unrolled-loop lo))))
	      (if (< c locycles)
		  (loop (+ lo 1) c lo)
		  (loop (+ lo 1) locycles lofactor)))
	    (list lofactor locycles)))))

(define (sim fn f)
  (call-with-input-file fn
    (lambda (p)
      (let loop ((l (read p)) (s 0))
	(if (null? l)
	    s
	    (loop (cdr l) (+ s (f (car l)))))))))

(define (unrolled-loop u)
  (lambda (n)
    (+ 3                                    ; setup
       (* (+ 3 u)                           ; cycles in body
	  (ceiling (/ n (* 8 u))))          ; number of iterations
       3)))                                 ; final

