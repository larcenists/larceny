; Copyright Lightship Software.
;
; CHAPTER.  Predicates.
;  (load "predicates.sch")
;(define eqv? eq?)
 
(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

; (eq? E K) compiles as (eq? K E),
; and (eq? E I) as (eq? I E), to make peephole optimization easier.
 
; (define-varying eq?
;   (lambda (l f)
;     (if (= (length l) 2)
;         (let ((e1 (cadr l))
;               (e2 (caddr l)))
;           (cond ((and (atom? e1)
;                       (not (symbol? e1)))
;                  l)
;                 ((and (atom? e2)
;                       (not (symbol? e2)))
;                  (list 'eq? e2 e1))
;                 ((symbol? e1) l)
;                 ((symbol? e2) (list 'eq? e2 e1))
;                 (else l)))
;         l))
;   (lambda (x y) (eq? x y)))

(define (equal? x y)
  (cond ((eqv? x y) #t)
	((and (pair? x) (pair? y))
	 (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
	((and (string? x) (string? y))
	 (string-equal? x y))
	((and (vector? x) (vector? y))
	 (vector-equal? x y))
	((and (bytevector? x) (bytevector? y))
	 (bytevector-equal? x y))
	(else #f)))
 
; Defined elsewhere for Larceny
;
;(define string=?
;  (lambda (s1 s2)
;    (bytevector-equal? (->bytevector s1) (->bytevector s2))))
 
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
 
; Defined elsewhere for Larceny.
;
; (define bytevector-equal?
;   (letrec ((bv-equal-loop
;              (lambda (bv1 bv2 i)
;                (cond ((<? i 0) #!true)
;                      ((=? (bytevector-ref bv1 i) (bytevector-ref bv2 i))
;                       (bv-equal-loop bv1 bv2 (1- i)))
;                      (else #!false)))))
;     (lambda (bv1 bv2)
;       (if (=? (bytevector-length bv1) (bytevector-length bv2))
;           (bv-equal-loop bv1 bv2 (1- (bytevector-length bv1)))
;           #!false))))
  
; List structure.
 
(define reverse
  (letrec ((reverse-loop
	    (lambda (l1 l2)
	      (if (null? l1)
		  l2
		  (reverse-loop (cdr l1) (cons (car l1) l2))))))
    (lambda (l)
      (reverse-loop l '()))))
 
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
		    ((null? (cddr args)) (apply append2 args))
		    (else (append2 (car args)
				   (apply append (cdr args))))))))
    append))
 
; This is not true for Larceny.
; (define memv memq)
; (define assv assq)
 
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
	((eqv? (car l) x) l)
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

; The current compiler hiccups on this.

; (define list?
;   (letrec ((loop (lambda (fast slow)
; 		   (cond ((null? fast) #t)
; 			 ((atom? fast) #f)
; 			 ((eq? fast slow) #f)
; 			 ((begin (set! fast (cdr fast))
; 				 (null? fast))
; 			  #t)
; 			 ((atom? fast) #f)
; 			 (else (loop (cdr fast) (cdr slow)))))))
;     (lambda (x)
;       (if (pair? x)
;           (loop (cdr x) x)
;           (null? x)))))

; (define proper-list? list?)

; Not IEEE

(define (list? x)
  (or (null? x)
      (and (pair? x) (list? (cdr x)))))
