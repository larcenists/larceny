; Lib/list.sch
; Larceny library -- list procedures
;
; $Id: list.sch,v 1.4 1997/05/15 00:42:10 lth Exp $
;
; Copyright 1991 Lightship Software
;
; Procedures of the form x->list and list->x can be found in the file for x.
;
; FIXME
;   In general we need to substantiate that the code generated for
;     (define (p a1 ...)
;        ...
;        (p e1 ...))
;   is as good as that generated for
;     (define (p a1 ...)
;        (define (p a1 ...)
;          ...
;          (p e1 ...))
;        (p a1 ...))
;   when benchmark-mode is turned on, and then rewrite all the procedures
;   in here that use the latter form into the former form, because they
;   look nicer.

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
 

(define (list . x) x)


(define (length l)

  (define (loop l len)
    (if (null? l)
	len
	(loop (cdr l) (+ len 1))))

  (loop l 0))


(define (map f x . rest)

  (define (map1 f x)
    (if (null? x)
	'()
	(cons (f (car x)) (map1 f (cdr x)))))

  (define (map2 f x y)
    (if (null? x)
	'()
	(cons (f (car x) (car y))
	      (map2 f (cdr x) (cdr y)))))

  (define (map3 f x y z)
    (if (null? x)
	'()
	(cons (f (car x) (car y) (car z))
	      (map3 f (cdr x) (cdr y) (cdr z)))))

  (define (map4 f x y z w)
    (if (null? x)
	'()
	(cons (f (car x) (car y) (car z) (car w))
	      (map4 f (cdr x) (cdr y) (cdr z) (cdr w)))))

  (define (mapn f lists)
    (if (null? (car lists))
	'()
	(cons (apply f (map1 car lists))
	      (mapn f (map1 cdr lists)))))

  (case (length rest)
    ((0)  (map1 f x))
    ((1)  (map2 f x (car rest)))
    ((2)  (map3 f x (car rest) (cadr rest)))
    ((3)  (map4 f x (car rest) (cadr rest) (caddr rest)))
    (else (mapn f (cons x rest)))))


(define (for-each f x . rest)

  (define (map1 f x)
    (if (null? x)
	'()
	(cons (f (car x)) (map1 f (cdr x)))))

  (define (for-each1 f x)
    (if (null? x)
	(unspecified)
	(begin (f (car x))
	       (for-each1 f (cdr x)))))

  (define (for-each2 f x y)
    (if (null? x)
	(unspecified)
	(begin (f (car x) (car y))
	       (for-each2 f (cdr x) (cdr y)))))

  (define (for-each3 f x y z)
    (if (null? x)
	(unspecified)
	(begin (f (car x) (car y) (car z))
	       (for-each3 f (cdr x) (cdr y) (cdr z)))))

  (define (for-each4 f x y z w)
    (if (null? x)
	(unspecified)
	(begin (f (car x) (car y) (car z) (car w))
	       (for-each4 f (cdr x) (cdr y) (cdr z) (cdr w)))))

  (define (for-each-n f lists)
    (if (null? (car lists))
	(unspecified)
	(begin (apply f (map1 car lists))
	       (for-each-n f (map1 cdr lists)))))

  (case (length rest)
    ((0)  (for-each1 f x))
    ((1)  (for-each2 f x (car rest)))
    ((2)  (for-each3 f x (car rest) (cadr rest)))
    ((3)  (for-each4 f x (car rest) (cadr rest) (caddr rest)))
    (else (for-each-n f (cons x rest)))))


(define reverse
  (letrec ((reverse-loop
	    (lambda (l1 l2)
	      (if (null? l1)
		  l2
		  (reverse-loop (cdr l1) (cons (car l1) l2))))))
    (lambda (l)
      (reverse-loop l '()))))


(define (reverse! l)
  (define (loop0 prev curr next)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop1 (cdr next) curr next)))
  (define (loop1 next prev curr)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop2 next (cdr next) curr)))
  (define (loop2 curr next prev)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop0 curr next (cdr next))))
  (if (null? l)
      '()
      (loop0 '() l (cdr l))))


; FIXME: the use of apply makes this slow in the n-ary case.

(define append
  (letrec ((append2
	    (lambda (x y)
	      (if (null? x)
		  y
		  (cons (car x) (append2 (cdr x) y)))))
           (append
	    (lambda args
	      (cond ((null? args) '())
		    ((null? (cdr args)) (car args))
		    ((null? (cddr args)) (append2 (car args) (cadr args)))
		    (else (append2 (car args) (apply append (cdr args))))))))
    append))


(define (append! . args)

  (define (loop rest tail)
    (cond ((null? rest)
	   tail)
	  ((null? (car rest))
	   (loop (cdr rest) tail))
	  (else
	   (loop (cdr rest)
		 (begin (set-cdr! (last-pair (car rest)) tail)
			(car rest))))))

  (if (null? args)
      '()
      (let ((a (reverse! args)))
	(loop (cdr a) (car a)))))


(define (list-tail x k)

  (define (list-tail x k)
    (if (zero? k)
	x
	(list-tail (cdr x) (- k 1))))

  (list-tail x k))


(define (list-ref x k)
  (car (list-tail x k)))


(define (list-set! l n o)
  (set-car! (list-tail l n) o))


(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))


; This is pretty much optimal for Larceny.

(define (list-copy l)
  (define (loop l prev)
    (if (null? l)
	#t
	(let ((q (cons (car l) '())))
	  (set-cdr! prev q)
	  (loop (cdr l) q))))
  (if (null? l)
      l
      (let ((first (cons (car l) '())))
	(loop (cdr l) first)
	first)))


(define member
  (letrec ((member
	    (lambda (x l)
	      (cond ((null? l) #f)
		    ((equal? x (car l)) l)
		    (else (member x (cdr l)))))))
    (lambda (x l)
      (cond ((symbol? x) (memq x l))
            ((number? x) (memv x l))
            (else (member x l))))))


(define (memv x l)
  (define (memv x l)
    (cond ((null? l) #f)
	  ((eqv? (car l) x) l)
	  (else (memv x (cdr l)))))
  (if (symbol? x)
      (memq x l)
      (memv x l)))


(define (memq x l)
  (cond ((null? l) #f)
	((eq? (car l) x) l)
	(else (memq x (cdr l)))))


(define assoc
  (letrec ((assoc
	    (lambda (x l)
	      (cond ((null? l) #f)
		    ((equal? x (caar l)) (car l))
		    (else (assoc x (cdr l)))))))
    (lambda (x l)
      (cond ((symbol? x) (assq x l))
            ((number? x) (assv x l))
            (else (assoc x l))))))


(define (assv x l)
  (define (assv x l)
      (cond ((null? l) #f)
	    ((eqv? (caar l) x) (car l))
	    (else (assv x (cdr l)))))
  (if (symbol? x) 
      (assq x l)
      (assv x l)))


(define (assq x l)
  (cond ((null? l) #f)
	((eq? (caar l) x) (car l))
	(else (assq x (cdr l)))))


(define (remove x l)
  (cond ((not (pair? l)) l)
	((equal? x (car l)) (remove x (cdr l)))
	(else (cons (car l) (remove x (cdr l))))))
 

(define (remv x l)
  (cond ((not (pair? l)) l)
	((eqv? x (car l)) (remv x (cdr l)))
	(else (cons (car l) (remv x (cdr l))))))


(define (remq x l)
  (cond ((not (pair? l)) l)
	((eq? x (car l)) (remq x (cdr l)))
	(else (cons (car l) (remq x (cdr l))))))


(define (list? x)
  (define (loop slow fast)
    (if (null? fast)
	#t
	(if (not (pair? fast))
	    #f
	    (if (eq? fast slow)
		#f
		(let ((fast (cdr fast)))
		  (if (null? fast)
		      #t
		      (if (not (pair? fast))
			  #f
			  (loop (cdr slow) (cdr fast)))))))))
  (or (null? x)
      (and (pair? x)
	   (loop x (cdr x)))))


; If all expressions are non-#f, then returns the value of the last
; expression, otherwise returns #f.  If list is null, return #t.

(define (every? p l)

  (define (loop l)
    (cond ((null? (cdr l)) (p (car l)))
	  ((p (car l)) (loop (cdr l)))
	  (else #f)))

  (if (null? l)
      #t
      (loop l)))


; Returns the value of the first non-#f expression, or #f if all are #f.

(define (some? p l)
  (cond ((null? l) #f)
	((p (car l)))
	(else (some? p (cdr l)))))


; Support for macro-expanded code (quasiquote).
; FIXME:  Really belongs somewhere else.

(define %append append)
(define %list list)
 

; eof
