; Auxlib/list.sch
; Larceny auxiliary library -- list functions.
;
; $Id: list.sch,v 1.1.1.1 1998/11/19 21:52:17 lth Exp $
;
; Compatibility notes:
; * Make-list, reduce, reduce-right, fold-right, fold-left are
;   compatible with MIT Scheme.
; * Make-list is compatible with Chez Scheme.

; These removal procedures work on any lists.

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

; These removal procedures work on assoc lists.

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

; Generalized selector -- returns a list of elements selected by
; the predicate.

(define (filter select? list)
  (cond ((null? list) list)
	((select? (car list))
	 (cons (car list) (filter select? (cdr list))))
	(else
	 (filter select? (cdr list)))))

; Generalized searcher -- returns the first element selected by
; the predicate.

(define (find selected? list)
  (cond ((null? list) #f)
	((selected? (car list)) (car list))
	(else (find selected? (cdr list)))))

; Return the least element of a list according to some total order.

(define (least less? l)
  (reduce (lambda (a b) (if (less? a b) a b)) #f l))

; Return the greatest element of a list according to some total order.

(define (greatest greater? l)
  (reduce (lambda (a b) (if (greater? a b) a b)) #f l))
  
; Map (proc : x -> list) over l, and append the resulting lists into 
; one list.

(define (mappend proc l)
  (apply append (map proc l)))

; (make-list n) => list of length n
; (make-list n x) => list of length n containing all x's

(define (make-list nelem . rest)
  (let ((val (if (null? rest) #f (car rest))))
    (define (loop n l)
      (if (zero? n)
	  l
	  (loop (- n 1) (cons val l))))
    (loop nelem '())))

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
	(proc (car l) (cadr l))
	(proc (car l) (loop (cdr l)))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop l))))

; (fold-left p x (a b ...)) => (p (p (p x a) b) ...)

(define (fold-left proc initial l)
  (if (null? l)
      initial
      (fold-left proc (proc initial (car l)) (cdr l))))

; (fold-right p x (a b ...)) => (p a (p b (p ... x)))

(define (fold-right proc initial l)
  (if (null? l)
      initial
      (proc (car l) (fold-right proc initial (cdr l)))))

; (iota n) => (0 1 2 ... n-1)

(define (iota n)
  (let loop ((n (- n 1)) (r '()))
    (let ((r (cons n r)))
      (if (= n 0)
	  r
	  (loop (- n 1) r)))))
	
; eof
