; Larceny/compat2.sch
; Larceny -- second part of compatibility code
;
; $Id: compat2.sch,v 1.1 1997/08/22 21:04:25 lth Exp $
;
; This file ought to be compiled.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Multiple values
; Larceny 0.25 does not have multiple values.

(define (values . x) x)

(define (call-with-values proc receiver)
  (apply receiver (proc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure.

(define compat:sort (lambda (list less?) (sort list less?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

(define (write-lop item port)
  (cond ((pair? item)
	 (compat:write-list item port))
	((vector? item) 
	 (write-char #\# port)
	 (compat:write-list (vector->list item) port))
	((bytevector? item)
	 (compat:write-bytevector item port))
	(else
	 (write item port))))

(define (compat:write-list item port)
  (write-char #\( port)
  (let loop ((item item))
    (cond ((null? item)
	   (write-char #\) port))
	  ((and (pair? item)
		(or (null? (cdr item))
		    (pair? (cdr item))))
	   (write-lop (car item) port)
	   (write-char #\space port)
	   (loop (cdr item)))
	  ((pair? item)
	   (write-lop (car item) port)
	   (display " . " port)
	   (write-lop (cdr item) port)
	   (write-char #\) port)))))
	  
; Compatible with Chez Scheme.

(define (compat:write-bytevector-as-vector item port)
  (let ((limit (bytevector-length item)))
    (display "#(" port)
    (do ((i 0 (+ i 1)))
	((= i limit) (write-char #\) port))
      (display (bytevector-ref item i) port)
      (display " " port))))

; Potentially much faster.

(define (compat:write-bytevector-as-bytevector item port)
  (let ((limit (bytevector-length item)))
    (write-char #\# port)
    (write-char (integer->char 2) port)
    (write-char #\" port)
    (do ((i 0 (+ i 1)))
	((= i limit) (write-char #\" port))
      (let ((c (integer->char (bytevector-ref item i))))
	(cond ((eq? c #\") (write-char #\\ port))
	      ((eq? c #\\) (write-char #\\ port)))
	(write-char c port)))))

; Default: fast

(define compat:write-bytevector compat:write-bytevector-as-bytevector)

; The power of self-hosting ;-)

(define (misc->bytevector x)
  (let ((bv (bytevector-like-copy x)))
    (typetag-set! bv $tag.bytevector-typetag)
    bv))

(define string->bytevector misc->bytevector)
(define bignum->bytevector misc->bytevector)

(define (flonum->bytevector x)
  (clear-first-word (misc->bytevector x)))

(define (compnum->bytevector x)
  (clear-first-word (misc->bytevector x)))

; Clears garbage word of compnum/flonum; makes regression testing much
; easier.

(define (clear-first-word bv)
  (bytevector-like-set! bv 0 0)
  (bytevector-like-set! bv 1 0)
  (bytevector-like-set! bv 2 0)
  (bytevector-like-set! bv 3 0)
  bv)

(define (list->bytevector l)
  (let ((b (make-bytevector (length l))))
    (do ((i 0 (+ i 1))
	 (l l (cdr l)))
	((null? l) b)
      (bytevector-set! b i (car l)))))

(define bytevector-word-ref 
  (let ((two^8  (expt 2 8))
	(two^16 (expt 2 16))
	(two^24 (expt 2 24)))
    (lambda (bv i)
      (+ (* (bytevector-ref bv i) two^24)
	 (* (bytevector-ref bv (+ i 1)) two^16)
	 (* (bytevector-ref bv (+ i 2)) two^8)
	 (bytevector-ref bv (+ i 3))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))

; eof
