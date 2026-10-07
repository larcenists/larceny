;;; Dead values in registers.
;;;
;;; The registers are roots, and a register keeps a dead value until
;;; something overwrites it.  This program leaves an 80MB vector in
;;; REG13 (the last argument of a 13-argument call), then runs a loop
;;; that uses only a few registers through many collections, and checks
;;; that a full collection frees the vector: the collector clears the
;;; registers that the code in REG0 does not use (clear_dead_registers
;;; in Rts/Sys/memmgr.c; the masks are made by the native assembler).
;;; With -keepregs the vector is kept.  Run it with
;;;
;;;     larceny -- dead-registers.sch
;;;     larceny -rrof -- dead-registers.sch
;;;
;;; It prints "dead-registers: ok" and exits (native Larceny; Petit does
;;; not clear registers, so it prints "dead-registers: kept").

(define (f13 a b c d e f g h i j k l m) a)
(set! f13 f13)                          ; not inlined

(define (f1 a) a)
(set! f1 f1)

(define (live-words)
  (collect)
  (memstats-heap-live-now (memstats)))

(define (churn n)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (f1 (make-vector 10 i))))

(define before (live-words))

(define (go)
  (f13 0 0 0 0 0 0 0 0 0 0 0 0 (make-vector 10000000 0))
  (churn 3000000)
  #t)

(go)

(define held (- (live-words) before))

(display "dead-registers: ")
(display (if (< held 1000000) "ok" "kept"))
(newline)
(exit)
