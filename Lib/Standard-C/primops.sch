; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- Procedure definitions for integrable procedures.
;
; NOTE! This file must be compiled with integrate-usual-procedures on.

($$trace "primops")

; General predicates

(define not (lambda (x) (not x)))
(define eq? (lambda (x y) (eq? x y)))
(define eqv? (lambda (x y) (eqv? x y)))

; Pairs and lists

(define null? (lambda (x) (null? x)))
(define pair? (lambda (x) (pair? x)))
(define car (lambda (x) (car x)))
(define cdr (lambda (x) (cdr x)))
(define cons (lambda (x y) (cons x y)))
(define set-car! (lambda (x y) (set-car! x y)))
(define set-cdr! (lambda (x y) (set-cdr! x y)))
'(define sys$partial-list->vector
  (lambda (x y) (sys$partial-list->vector x y)))

; Numbers

(define number? (lambda (x) (number? x)))
(define complex? (lambda (x) (complex? x)))
(define real? (lambda (x) (real? x)))
(define rational? (lambda (x) (rational? x)))
(define integer? (lambda (x) (integer? x)))
(define fixnum? (lambda (x) (fixnum? x)))
(define flonum? (lambda (x) (flonum? x)))
(define compnum? (lambda (x) (compnum? x)))
(define exact? (lambda (x) (exact? x)))
(define inexact? (lambda (x) (inexact? x)))
(define exact->inexact (lambda (x) (exact->inexact x)))
(define inexact->exact (lambda (x) (inexact->exact x)))
(define round (lambda (x) (round x)))
(define truncate (lambda (x) (truncate x)))
(define zero? (lambda (x) (zero? x)))
(define -- (lambda (x) (-- x)))
(define lognot (lambda (x) (lognot x)))
(define real-part (lambda (x) (real-part x)))
(define imag-part (lambda (x) (imag-part x)))
(define quotient (lambda (x y) (quotient x y)))
(define logand (lambda (x y) (logand x y)))
(define logior (lambda (x y) (logior x y)))
(define logxor (lambda (x y) (logxor x y)))
(define lsh (lambda (x y) (lsh x y)))
(define rshl (lambda (x y) (rshl x y)))
(define rsha (lambda (x y) (rsha x y)))
; (define rot (lambda (x y) (rot x y)))
(define remainder (lambda (x y) (remainder x y)))

(define +
  (letrec ((loop (lambda (sum args)
		   (if (null? args)
		       sum
		       (loop (+ sum (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  0
	  (loop (car args) (cdr args))))))

(define - 
  (letrec ((loop (lambda (diff args)
		   (if (null? args)
		       diff
		       (loop (- diff (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (-- arg)
	  (loop arg args)))))

(define * 
  (letrec ((loop (lambda (prod args)
		   (if (null? args)
		       prod
		       (loop (* prod (car args)) (cdr args))))))
    (lambda args
      (if (null? args)
	  1
	  (loop (car args) (cdr args))))))

(define /
  (letrec ((loop (lambda (quot args)
		   (if (null? args)
		       quot
		       (loop (/ quot (car args)) (cdr args))))))
    (lambda (arg . args)
      (if (null? args)
	  (/ 1 arg)
	  (loop arg args)))))

(define < 
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((< first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (< a b)
	  (and (< a b)
	       (loop b rest))))))

(define <= 
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((<= first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (<= a b)
	  (and (<= a b)
	       (loop b rest))))))

(define = 
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((= first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (= a b)
	  (and (= a b)
	       (loop b rest))))))

(define >
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((> first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (> a b)
	  (and (> a b)
	       (loop b rest))))))

(define >= 
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((>= first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (>= a b)
	  (and (>= a b)
	       (loop b rest))))))


; Characters

(define char? (lambda (x) (char? x)))
(define char->integer (lambda (x) (char->integer x)))
(define integer->char (lambda (x) (integer->char x)))
(define char<? (lambda (x y) (char<? x y)))
(define char<=? (lambda (x y) (char<=? x y)))
(define char=? (lambda (x y) (char=? x y)))
(define char>? (lambda (x y) (char>? x y)))
(define char>=? (lambda (x y) (char>=? x y)))

; Strings

(define make-string
  (lambda (x . rest)
    (if (null? rest)
	(make-string x #\space)
	(make-string x (car rest)))))
(define string? (lambda (x) (string? x)))
(define string-length (lambda (x) (string-length x)))
(define string-ref (lambda (x y) (string-ref x y)))
(define string-set! (lambda (x y z) (string-set! x y z)))

; Vectors

(define make-vector
  (lambda (x . rest) 
    (if (null? rest)
	(make-vector x #f)
	(make-vector x (car rest)))))
(define vector? (lambda (x) (vector? x)))
(define vector-length (lambda (x) (vector-length x)))
(define vector-ref (lambda (x y) (vector-ref x y)))

; Vector-like

(define vector-set! (lambda (x y z) (vector-set! x y z)))
(define vector-like? (lambda (x) (vector-like? x)))
(define vector-like-ref (lambda (x y) (vector-like-ref x y)))
(define vector-like-set! (lambda (x y z) (vector-like-set! x y z)))
(define vector-like-length (lambda (x) (vector-like-length x)))

; Bytevectors

(define bytevector? (lambda (x) (bytevector? x)))
(define bytevector-length (lambda (x) (bytevector-length x)))
(define make-bytevector (lambda (x) (make-bytevector x)))
(define bytevector-ref (lambda (x y) (bytevector-ref x y)))
(define bytevector-set! (lambda (x y z) (bytevector-set! x y z)))
(define bytevector-fill! (lambda (x y) (bytevector-fill! x y)))

; Bytevector-like

(define bytevector-like? (lambda (x) (bytevector-like? x)))
(define bytevector-like-ref (lambda (x y) (bytevector-like-ref x y)))
(define bytevector-like-set! (lambda (x y z) (bytevector-like-set! x y z)))
(define bytevector-like-length (lambda (x) (bytevector-like-length x)))

(define bytevector-like-compare
  (lambda (x y)
    (if (and (bytevector-like? x)
	     (bytevector-like? y))
	(sys$bvlcmp x y)
	(begin (error "bytevector-like-compare: bad arguments: " x " " y)
	       #t))))

; Structures and typetags

(define structure? (lambda (x) (structure? x)))
(define typetag (lambda (x) (typetag x)))
(define typetag-set! (lambda (x y) (typetag-set! x y)))

; Procedures

(define procedure? (lambda (x) (procedure? x)))
(define procedure-length (lambda (x) (procedure-length x)))
(define make-procedure (lambda (x) (make-procedure x)))
(define procedure-ref (lambda (x y) (procedure-ref x y)))
(define procedure-set! (lambda (x y z) (procedure-set! x y z)))

; Cells

(define make-cell (lambda (x) (make-cell x)))
(define cell-ref (lambda (x) (cell-ref x)))
(define cell-set! (lambda (x y) (cell-set! x y)))

; I/O system support

(define port? (lambda (x) (port? x)))
(define eof-object (lambda () (eof-object)))
(define eof-object? (lambda (x) (eof-object? x)))

; Miscellaneous

(define symbol? (lambda (x) (symbol? x)))
(define break (lambda () (break)))
(define creg (lambda () (creg)))
(define creg-set! (lambda (x) (creg-set! x)))
(define undefined (lambda () (undefined)))
(define unspecified (lambda () (unspecified)))
(define enable-interrupts (lambda (n) (enable-interrupts n)))
(define disable-interrupts (lambda () (disable-interrupts)))
(define gc-counter (lambda () (gc-counter)))

; eof
