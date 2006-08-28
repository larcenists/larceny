; Copyright 1998 William D Clinger.
;
; $Id$
;
; Call count profiling of top-level named procedures.
; This code must be compiled, with integrate-usual-procedures set to #t.

; Requires
;    Auxlib/sort.sch        [ sort procedure ]

(define begin-countcalls)
(define (end-countcalls) #f) ; assigned by begin-countcalls
(define (report-calls) #f)   ; assigned by begin-countcalls

(let* ((counting? #f)
       (namespace (lambda ()
		    (environment-variables (interaction-environment))))
       (apply apply)
       (environment-get-cell environment-get-cell)
       (interaction-environment interaction-environment)
       (symeval   (lambda (sym)
		    (car (environment-get-cell
			  (interaction-environment) sym))))
       (symset!   (lambda (sym x)
		    (set-car! (environment-get-cell
			       (interaction-environment) sym) x))))
  (define (filter-loop p? x y)
    (cond ((null? x) y)
          ((p? (car x))
           (filter-loop p? (cdr x) (cons (car x) y)))
          (else
           (filter-loop p? (cdr x) y))))
  (set! begin-countcalls
        (lambda ()
          (if (not counting?)
              (let ((variables '#())
                    (originals '#())
                    (calls '#())
                    (n 0))
                (define (f i original)
                  (lambda args
                    (vector-set! calls i (+ (vector-ref calls i) 1))
                    (apply original args)))
                (set! end-countcalls
                      (lambda ()
                        (set! counting? #f)
                        (do ((i 0 (+ i 1)))
                            ((= i n) #t)
                            (symset! (vector-ref variables i)
                                     (vector-ref originals i)))))
                (set! report-calls
                      (lambda ()
                        (define (iota n)
                          (do ((n (- n 1) (- n 1))
                               (x '() (cons n x)))
                              ((negative? n) x)))
                        (define (right-justify x n)
                          (let ((s (number->string x)))
                            (if (< (string-length s) n)
                                (display
                                 (make-string (- n (string-length s))
                                              #\space)))
                            (display s)))
                        (let ((indexes
			       (sort (iota n)
				     (lambda (i j)
				       (let ((ci (vector-ref calls i))
					     (cj (vector-ref calls j)))
					 (or (> ci cj)
					     (and (= ci cj)
						  (string<?
						   (symbol->string
						    (vector-ref variables i))
						   (symbol->string
						    (vector-ref variables j)))))))))
                              (total (do ((i 0 (+ i 1))
                                          (total 0 (+ total
                                                      (vector-ref calls i))))
                                         ((= i n) total))))
                          (newline)
                          (display total)
                          (display " calls to top-level procedures")
                          (newline)
                          (newline)
                          (for-each (lambda (i)
                                      (let ((z (vector-ref calls i)))
                                        (if (positive? z)
                                            (begin
                                             (right-justify z 6)
                                             (right-justify
                                              (/ (round (/ (* 1000 z) total))
                                                 10.0)
                                              6)
                                             (display "  ")
                                             (write (vector-ref variables i))
                                             (newline)))))
                                    indexes))))
                (set! variables
                      (list->vector
                       (filter-loop (lambda (var)
                                      (procedure? (symeval var)))
                                    (namespace)
                                    '())))
                (set! originals
                      (list->vector
                       (map symeval
                            (vector->list variables))))
                (set! n (vector-length variables))
                (set! calls (make-vector n 0))
                (set! counting? #t)  ; WAS: a bug
                (do ((i 0 (+ i 1)))
                    ((= i n) #t)
                    (symset! (vector-ref variables i)
                             (f i (vector-ref originals i)))))
              #f))))

;eof
