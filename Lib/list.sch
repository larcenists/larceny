; Copyright 1991 Lightship Software
;
; Larceny library -- list procedures
;
; $Id: list.sch,v 1.2 1997/02/03 20:07:13 lth Exp $
;
; Procedures of the form x->list and list->x can be found in the file for x.

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
 
(define list (lambda x x))

(define %list list)
 
(define (length l)
  (letrec ((loop (lambda (l len)
		   (if (null? l)
		       len
		       (loop (cdr l) (+ len 1))))))
    (loop l 0)))

(define for-each)
(define map)
(letrec
  ((map2 (lambda (f l)
           (if (null? l) '() (cons (f (car l)) (map2 f (cdr l))))))
   (for-each2 (lambda (f l)
                (if (null? l)
                    #f
                    (begin (f (car l))
                           (for-each2 f (cdr l))))))
   (map0 (lambda (f l1 . rest)
           (cond ((null? l1) '())
                 (else (cons (apply f (cons (car l1) (map2 car rest)))
                             (apply map0 (help0 f l1 rest)))))))
   (for-each0 (lambda (f l1 . rest)
                (cond ((null? l1) #f)
                      (else (apply f (cons (car l1) (map2 car rest)))
                            (apply for-each0 (help0 f l1 rest))))))
   (help0 (lambda (f l1 rest)
            (cons f (cons (cdr l1) (map2 cdr rest)))))
   )
  (set! map
        (lambda (f l1 . rest)
          (if (null? rest)
              (map2 f l1)
              (apply map0 (cons f (cons l1 rest))))))
  (set! for-each
        (lambda (f l1 . rest)
          (if (null? rest)
              (for-each2 f l1)
              (apply for-each0 (cons f (cons l1 rest))))))
  #t)

; List structure.
 
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


; Slow; should use a better algorithm.

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

; For macro expander.

(define %append append)

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


(define list-tail
  (letrec ((list-tail
             (lambda (x k)
               (if (zero? k)
                   x
                   (list-tail (cdr x) (- k 1))))))
    (lambda (x k)
      (list-tail x k))))

(define list-ref
  (lambda (x k)
    (car (list-tail x k))))

(define list-set!
  (lambda (l n o)
    (set-car! (list-tail l n) o)))

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
  (letrec ((member (lambda (x l)
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
  (letrec ((assoc (lambda (x l)
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
  (cond ((atom? l) l)
	((equal? x (car l)) (remove x (cdr l)))
	(else (cons (car l) (remove x (cdr l))))))
 
(define (remv x l)
  (cond ((atom? l) l)
	((eqv? x (car l)) (remv x (cdr l)))
	(else (cons (car l) (remv x (cdr l))))))

(define (remq x l)
  (cond ((atom? l) l)
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

; eof
