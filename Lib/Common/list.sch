; Copyright 1991 Lightship Software
;
; $Id$
;
; Larceny library -- list procedures.
;
; Procedures of the form x->list and list->x can be found in the file for x.

($$trace "list")

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


; Probably due to JonL White.

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


(define (append . args)

  (define (loop rest tail)
    (cond ((null? rest)
	   tail)
	  ((null? (car rest))
	   (loop (cdr rest) tail))
	  (else
	   (loop (cdr rest)
                 (call-with-values 
                  (lambda () 
                    (list-copy2 (car rest)))
                  (lambda (new-head new-tail)
                    (set-cdr! new-tail tail)
                    new-head))))))

  (if (null? args)
      '()
      (let ((a (reverse! args)))
	(loop (cdr a) (car a)))))


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


; Returns both the first and last pairs of the argument, or (),() if the
; argument is ().

(define (list-copy2 l)
  (define (loop l prev)
    (if (null? l)
	prev
	(let ((q (cons (car l) '())))
	  (set-cdr! prev q)
	  (loop (cdr l) q))))
  (if (null? l)
      (values l l)
      (let ((first (cons (car l) '())))
	(values first (loop (cdr l) first)))))


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


(define (remq! key list)
  (cond ((null? list) list)
	((eq? key (car list))
	 (remq! key (cdr list)))
	(else
	 (set-cdr! list (remq! key (cdr list)))
	 list)))


(define (remv! key list)
  (cond ((null? list) list)
	((eqv? key (car list))
	 (remv! key (cdr list)))
	(else
	 (set-cdr! list (remv! key (cdr list)))
	 list)))


(define (remove! key list)
  (cond ((null? list) list)
	((equal? key (car list))
	 (remove! key (cdr list)))
	(else
	 (set-cdr! list (remove! key (cdr list)))
	 list)))


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


(define (every? p l . ls)

  (define (every1 a)
    (cond ((null? (cdr a)) (p (car a)))
	  ((p (car a)) (every1 (cdr a)))
	  (else #f)))

  (define (every2 a b)
    (cond ((null? (cdr a)) (p (car a) (car b)))
	  ((p (car a) (car b)) (every2 (cdr a) (cdr b)))
	  (else #f)))

  (define (every3 a b c)
    (cond ((null? (cdr a)) (p (car a) (car b) (car c)))
	  ((p (car a) (car b) (car c)) (every3 (cdr a) (cdr b) (cdr c)))
	  (else #f)))

  (define (every-n ls)
    (cond ((null? (cdar ls)) (apply p (map car ls)))
          ((apply p (map car ls)) (every-n (map cdr ls)))
          (else #f)))

  (cond ((null? ls)
         (or (null? l) (every1 l)))
        ((null? (cdr ls))
         (or (null? l) (every2 l (car ls))))
        ((null? (cddr ls))
         (or (null? l) (every3 l (car ls) (cadr ls))))
        (else
         (or (null? l) (every-n (cons l ls))))))


(define (some? p l . ls)

  (define (some1 a)
    (cond ((null? a) #f)
          ((p (car a)))
          (else (some1 (cdr a)))))

  (define (some2 a b)
    (cond ((null? a) #f)
          ((p (car a) (car b)))
          (else (some2 (cdr a) (cdr b)))))

  (define (some3 a b c)
    (cond ((null? a) #f)
          ((p (car a) (car b) (car c)))
          (else (some3 (cdr a) (cdr b) (cdr c)))))

  (define (some-n ls)
    (cond ((null? (car ls)) #f)
          ((apply p (map car ls)))
          (else (some-n (map cdr ls)))))

  (cond ((null? ls) (some1 l))
        ((null? (cdr ls)) (some2 l (car ls)))
        ((null? (cddr ls)) (some3 l (car ls) (cadr ls)))
        (else (some-n (cons l ls)))))

; eof
