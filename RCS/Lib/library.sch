; Copyright 1991 Lightship Software
;
; Standard Scheme procedures needed by print.sch,
; rewritten somewhat for MacScheme Version 4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                              ;
; Portable code.                                               ;
;                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; from list.sch
 
(set! caar (lambda (x) (car (car x))))
(set! cadr (lambda (x) (car (cdr x))))
(set! cdar (lambda (x) (cdr (car x))))
(set! cddr (lambda (x) (cdr (cdr x))))
(set! caaar (lambda (x) (car (car (car x)))))
(set! caadr (lambda (x) (car (car (cdr x)))))
(set! cadar (lambda (x) (car (cdr (car x)))))
(set! caddr (lambda (x) (car (cdr (cdr x)))))
(set! cdaar (lambda (x) (cdr (car (car x)))))
(set! cdadr (lambda (x) (cdr (car (cdr x)))))
(set! cddar (lambda (x) (cdr (cdr (car x)))))
(set! cdddr (lambda (x) (cdr (cdr (cdr x)))))
(set! caaaar (lambda (x) (car (car (car (car x))))))
(set! caaadr (lambda (x) (car (car (car (cdr x))))))
(set! caadar (lambda (x) (car (car (cdr (car x))))))
(set! caaddr (lambda (x) (car (car (cdr (cdr x))))))
(set! cadaar (lambda (x) (car (cdr (car (car x))))))
(set! cadadr (lambda (x) (car (cdr (car (cdr x))))))
(set! caddar (lambda (x) (car (cdr (cdr (car x))))))
(set! cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(set! cdaaar (lambda (x) (cdr (car (car (car x))))))
(set! cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(set! cdadar (lambda (x) (cdr (car (cdr (car x))))))
(set! cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(set! cddaar (lambda (x) (cdr (cdr (car (car x))))))
(set! cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(set! cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(set! cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
 
(set! list (lambda x x))
 
; from control.sch

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

; from vector.sch

(define vector
  (lambda l
    (list->vector l)))

(define list->vector
  (letrec ((loop
             (lambda (v i l)
               (if (not (null? l))
                   (begin (vector-set! v i (car l))
                          (loop v (+ i 1) (cdr l)))
                   v))))
    (lambda (l)
      (loop (make-vector (length l) '()) 0 l))))

(define vector->list
  (letrec ((loop
             (lambda (v i l)
               (if (< i 0)
                   l
                   (loop v (- i 1) (cons (vector-ref v i) l))))))
    (lambda (v)
      (loop v (- (vector-length v) 1) '()))))

; from number.sch

; (define remainder
;   (lambda (n modulus)
;     (- n (* modulus (quotient n modulus)))))

; from preds.sch (go figure!)

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

(define list?
  (letrec
    ((loop (lambda (fast slow)
             (cond ((null? fast) #t)
                   ((not (pair? fast)) #f)
                   ((eq? fast slow) #f)
                   (else (let ((fast (cdr fast)))
                           (cond ((null? fast) #t)
                                 ((not (pair? fast)) #f)
                                 (else (loop (cdr fast) (cdr slow))))))))))
    (lambda (x)
      (if (pair? x)
          (loop (cdr x) x)
          (null? x)))))

; from misc.sch
; @raw-apply@ is written in mal (MacScheme assembly language)
; in another file.  It takes exactly two arguments and does
; something magical with them.

(define apply
  (let ((raw-apply @raw-apply@))              ; see above
    (define (loop x rest)
      (if (null? (cdr rest))
          (append (reverse x) (car rest))
          (loop (cons (car rest) x) (cdr rest))))
    (define (apply f l . rest)
      (cond ((not (procedure? f))
             (error "Non-procedure -- apply" f))
            ((not (null? rest))
             (apply f (loop (list l) rest)))
            ((null? l) (f))
            ((null? (cdr l)) (f (car l)))
            ((null? (cddr l)) (f (car l) (cadr l)))
            ((null? (cdddr l)) (f (car l) (cadr l) (caddr l)))
            ((null? (cddddr l)) (f (car l) (cadr l) (caddr l) (cadddr l)))
            ((not (list? l))
             (error "Non-list -- apply" l))
            (else (raw-apply f l))))
    apply))

; from make25.sch

(define call-without-interrupts
  (lambda (thunk)
    (thunk)))

; from error.sch (but completely rewritten!)

(define error
  (lambda args
    (display "error.") (newline)
    (for-each (lambda (x) (display x) (display " ")) args)
    (newline)
    (display "entering debugger.") (newline)
    (debugvsm)))
