; Larceny auxiliary library -- miscellaneous
; $Id: misc.sch,v 1.2 1997/08/22 20:58:12 lth Exp $

(define *pi* 3.14159265358979323846)           ; from <math.h>

; Names used by older code.

(define some some?)
(define every every?)

; Names used in Chez Scheme

(define andmap every?)
(define ormap some?)

; Not as efficient as it could be!

(define (remq! key list)
  (cond ((null? list) list)
	((eq? key (car list))
	 (remq! key (cdr list)))
	(else
	 (set-cdr! list (remq! key (cdr list))))))

; MIT Scheme has this.

(define (make-list nelem val)
  (if (zero? nelem)
      '()
      (cons val (make-list (- nelem 1) val))))

; Reductions.
; The procedures optionally take an identity element.  If the
; identity is present, lists of length 0 are allowed.  If not,
; the list must have at least one element.
;
; FIXME: These need to be compatible with MIT Scheme; check the docs.

(define (reduce-left proc l . rest)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (if (and (null? rest) (null? l))
      (error "reduce-left: not enough arguments."))
  (if (null? rest)
      (loop (car l) (cdr l))
      (loop (car rest) l)))

(define (reduce-right proc l . rest)

  (define (loop val l)
    (if (null? l)
	(if (null? rest)
	    val
	    (proc val (car rest)))
	(proc val (loop (car l) (cdr l)))))
	
  (if (and (null? rest) (null? l))
      (error "reduce-right: not enough arguments."))
  (if (null? l)
      (car rest)
      (loop (car l) (cdr l))))

; Should be in the basis library?

(define (vector-copy v)
  (let ((v2 (make-vector (vector-length v) #f)))
    (do ((i (- (vector-length v) 1) (- i 1)))
        ((< i 0) v2)
      (vector-set! v2 i (vector-ref v i)))))

; eof
