; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Useful list functions.
;
; Notes:
; * Reduce, reduce-right, fold-right, fold-left are compatible with MIT Scheme.
; * Make-list is compatible with MIT Scheme and Chez Scheme.
; * These are not (yet) compatible with Shivers's proposed list functions.
; * find, filter, fold-left, fold-right are now in Lib/Common/list.sch
; * remq, remv, remove, remq!, remv!, remove!, every?, and some?
;   have long been in Lib/Common/list.sch

; Destructively remove all associations whose key matches `key' from `alist'.

(define (aremq! key alist)
  (cond ((null? alist) alist)
	((eq? key (caar alist))
	 (aremq! key (cdr alist)))
	(else
	 (set-cdr! alist (aremq! key (cdr alist)))
	 alist)))

(define (aremv! key alist)
  (cond ((null? alist) alist)
	((eqv? key (caar alist))
	 (aremv! key (cdr alist)))
	(else
	 (set-cdr! alist (aremv! key (cdr alist)))
	 alist)))

(define (aremove! key alist)
  (cond ((null? alist) alist)
	((equal? key (caar alist))
	 (aremove! key (cdr alist)))
	(else
	 (set-cdr! alist (aremove! key (cdr alist)))
	 alist)))

; Assq etc on the cdr of the entry.

(define (reverse-assq key alist)
  (cond ((null? alist) #f)
        ((eq? (cdar alist) key) (car alist))
        (else (reverse-assq key (cdr alist)))))

(define (reverse-assv key alist)
  (cond ((null? alist) #f)
        ((eqv? (cdar alist) key) (car alist))
        (else (reverse-assv key (cdr alist)))))

(define (reverse-assoc key alist)
  (cond ((null? alist) #f)
        ((equal? (cdar alist) key) (car alist))
        (else (reverse-assoc key (cdr alist)))))

; Map proc over lists and return all non-#f values.

(define (filter-map proc . lists)
  (filter (lambda (x) x)
          (apply map proc lists)))

; Return a list with all duplicates (according to predicate) removed.

(define (remove-duplicates list same?)

  (define (member? x list)
    (cond ((null? list) #f)
          ((same? x (car list)) #t)
          (else (member? x (cdr list)))))

  (cond ((null? list) list)
        ((member? (car list) (cdr list))
         (remove-duplicates (cdr list) same?))
        (else
         (cons (car list) (remove-duplicates (cdr list) same?)))))

; Return the least element of `list' according to some total order.

(define (least less? list)
  (reduce (lambda (a b) (if (less? a b) a b)) #f list))

; Return the greatest element of `list' according to some total order.

(define (greatest greater? list)
  (reduce (lambda (a b) (if (greater? a b) a b)) #f list))
  
; (mappend p l) = (apply append (map p l))

(define (mappend proc l)
  (apply append (map proc l)))

; (make-list n)   => (a1 ... an) for some ai
; (make-list n x) => (a1 ... an) where ai = x
;
; This is a standard procedure in R7RS.
;
;(define (make-list nelem . rest)
;  (let ((val (if (null? rest) #f (car rest))))
;    (define (loop n l)
;      (if (<= n 0)
;	  l
;	  (loop (- n 1) (cons val l))))
;    (loop nelem '())))

; (reduce p x ()) => x
; (reduce p x (a)) => a
; (reduce p x (a b ...)) => (p (p a b) ...))

(define (reduce proc initial l)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop (car l) (cdr l)))))

; (reduce-right p x ()) => x
; (reduce-right p x (a)) => a
; (reduce-right p x (a b ...)) => (p a (p b ...))

(define (reduce-right proc initial l)

  (define (loop l)
    (if (null? (cdr l))
	(car l)
	(proc (car l) (loop (cdr l)))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop l))))

; (iota n) => (0 1 2 ... n-1)

(define (iota n)
  (let loop ((n (- n 1)) (r '()))
    (let ((r (cons n r)))
      (if (= n 0)
	  r
	  (loop (- n 1) r)))))

; (list-head (a1 ... an) m) => (a1 ... am)   for m <= n

(define (list-head l n)
  (if (zero? n)
      '()
      (cons (car l) (list-head (cdr l) (- n 1)))))
	
; eof
