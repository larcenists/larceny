; Auxlib/misc.sch
; Larceny auxiliary library -- miscellaneous
;
; $Id: misc.sch,v 1.2 1997/08/22 20:58:12 lth Exp $

;;; System information

; Name proposed by Marc Feeley.

(define (scheme-system) 
  (let ((x   (open-output-string))
	(inf (system-features)))
    (display "Larceny Version ")
    (display (cdr (assq 'larceny-major-version inf)) x)
    (display "." x)
    (display (cdr (assq 'larceny-minor-version inf)) x)
    (get-output-string x)))

;;; Constants

(define *pi* 3.14159265358979323846)

;;; Lists

; These work on any lists.

(define (remq! key list)
  (cond ((null? list) list)
	((eq? key (car list))
	 (remq! key (cdr list)))
	(else
	 (set-cdr! list (remq! key (cdr list))))))

(define (remv! key list)
  (cond ((null? list) list)
	((eqv? key (car list))
	 (remv! key (cdr list)))
	(else
	 (set-cdr! list (remv! key (cdr list))))))

(define (remove! key list)
  (cond ((null? list) list)
	((equal? key (car list))
	 (remove! key (cdr list)))
	(else
	 (set-cdr! list (remove! key (cdr list))))))

; These work on assoc lists.

(define (aremq! key list)
  (cond ((null? list) list)
	((eq? key (caar list))
	 (aremq! key (cdr list)))
	(else
	 (set-cdr! list (aremq! key (cdr list))))))

(define (aremv! key list)
  (cond ((null? list) list)
	((eqv? key (caar list))
	 (aremv! key (cdr list)))
	(else
	 (set-cdr! list (aremv! key (cdr list))))))

(define (aremove! key list)
  (cond ((null? list) list)
	((equal? key (caar list))
	 (aremove! key (cdr list)))
	(else
	 (set-cdr! list (aremove! key (cdr list))))))

; Generalized selector

(define (filter select? list)
  (cond ((null? list) list)
	((select? (car list))
	 (cons (car list) (filter select? (cdr list))))
	(else
	 (filter select? (cdr list)))))

; Generalized searcher

(define (find selected? list)
  (cond ((null? list) #f)
	((selected? (car list)) (car list))
	(else (find selected? (cdr list)))))

; Make-list, reduce, reduce-right, fold-right, fold-left are compatible
; with MIT Scheme.  Make-list is also compatible with Chez Scheme.

(define (make-list nelem . rest)
  (let ((val (if (null? rest) #f (car rest))))
    (define (loop n l)
      (if (zero? n)
	  l
	  (loop (- n 1) (cons val l))))
    (loop nelem '())))

(define (reduce proc initial l)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop (car l) (cdr l)))))

(define (reduce-right proc initial l)

  (define (loop l)
    (if (null? (cdr l))
	(proc (car l) (cadr l))
	(proc (car l) (loop (cdr l)))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop l))))

(define (fold-left proc initial l)
  (if (null? l)
      initial
      (fold-left proc (proc initial (car l)) (cdr l))))

(define (fold-right proc initial l)
  (if (null? l)
      initial
      (proc (car l) (fold-right proc initial (cdr l)))))

;;; Vectors

; Should be in the basis library?

(define (vector-copy v)
  (let ((v2 (make-vector (vector-length v) #f)))
    (do ((i (- (vector-length v) 1) (- i 1)))
        ((< i 0) v2)
      (vector-set! v2 i (vector-ref v i)))))

; eof
