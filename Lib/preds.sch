; Copyright Lightship Software.
;
; Modified 950711 / lth:
;    Added reverse!, append!.
;
; Modified 950611 / lth:
;    Memq now uses eq? (rather than eqv?).
;
; Modified 950528 / lth: 
;    Added IEEE-compliant list?, and last-pair.
;
; Modified 15 March by Will Clinger:
;    added list-tail, list-ref
;
; CHAPTER.  Predicates.
;  (load "predicates.sch")
 
(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

(define (equal? x y)
  (cond ((eqv? x y) #t)
	((and (pair? x) (pair? y))
	 (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
	((and (string? x) (string? y))
	 (string=? x y))
	((and (vector? x) (vector? y))
	 (vector-equal? x y))
	((and (bytevector? x) (bytevector? y))
	 (bytevector-equal? x y))
	(else #f)))
 
(define string-equal? string=?)       ; for backward compatibility
 
(define vector-equal?
  (letrec ((v-equal-loop
	    (lambda (v1 v2 i)
	      (cond ((<? i 0) #t)
		    ((equal? (vector-ref v1 i) (vector-ref v2 i))
		     (v-equal-loop v1 v2 (1- i)))
		    (else #f)))))
    (lambda (v1 v2)
      (if (=? (vector-length v1) (vector-length v2))
          (v-equal-loop v1 v2 (1- (vector-length v1)))
          #f))))
 
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

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

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
