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


; List->vector is a partial primop in larceny for the purposes of the
; thesis. The theory is that we can initialize the vector much more
; efficiently if we don't have to check for intergenerational pointers
; all the time, and we can do this when we know that the vector is in
; the tenured area.

(define list->vector
  (lambda (l)
    (sys$partial-list->vector l (length l))))


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
