; Larceny run-time library.
; System-specific library routines and global variables.
;
; $Id: xlib.sch,v 1.1 91/09/14 01:20:35 lth Exp Locker: lth $

; Typetags for bytevector-like structures.
; The typetags really should be gotten from the auto-generated stuff.

(define sys$tag.bytevector-typetag 0)
(define	sys$tag.string-typetag 1)
(define sys$tag.flonum-typetag 2)
(define sys$tag.compnum-typetag 3)
(define sys$tag.bignum-typetag 4)

; Typetags for vector-like structures.

(define sys$tag.vector-typetag 0)
(define sys$tag.rectnum-typetag 1)
(define sys$tag.ratnum-typetag 2)
(define sys$tag.symbol-typetag 3)
(define sys$tag.port-typetag 4)

; These are integrable in MacScheme but not in Larceny.

(define (length l)
  (letrec ((loop (lambda (l len)
		   (if (null? l)
		       len
		       (loop (cdr l) (+ len 1))))))
    (loop l 0)))

(define (memq obj l)
  (letrec ((loop (lambda (obj l)
		   (cond ((null? l) #f)
			 ((eq? obj (car l)) l)
			 (else (loop obj (cdr l)))))))
    (loop obj l)))

(define (assq obj l)
  (letrec ((loop (lambda (obj l)
		   (cond ((null? l) #f)
			 ((eq? obj (car (car l))) (car l))
			 (else (loop obj (cdr l)))))))
    (loop obj l)))

; Rudimentary.
; This works for positive integer b only, which is all we need to bootstrap.
; It is redefined later.

(define (expt a b)
  (if (> b 1)
      (* a (expt a (- b 1)))
      a))

(define (call-with-current-continuation proc)
  (let ((k (creg)))
    (proc (lambda (v) 
;	    (write 'throwing) (write v) (newline)
;	    (print-continuation k)
;	    (break)
	    (creg-set! k)
	    v))))

; a continuation is a vector; we print it in a slightly more palatable
; format by avoiding printing all the saved data slots.

(define (print-continuation k)
  (let loop ((l (vector-length k)) (i 0))
    (if (= i l)
	(if (vector-ref k 0)
	    (begin (newline)
		   (print-continuation (vector-ref k 0))))
	(let ((x (vector-ref k i)))
	  (if (not (or (vector? x) (pair? x)))
	      (begin (display x) (newline))
	      (begin (display "#<complex>") (newline)))
	  (loop l (+ i 1))))))


