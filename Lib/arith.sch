;                               17 April 1990
;
; Uses flonums-package.
 
(load "import.sch")
 
(load "access.sch")
 
(letrec
  (
   
   ;(bignum?            (access 'bignum? flonums-package))
   
   (flonum?            (access 'flonum? flonums-package))   
   (flo-zero           (access 'flo-zero flonums-package))
   (number->flonum     (access 'number->flonum flonums-package))
   (flo/               (access 'flo/ flonums-package))
   ;(number->exact      (access 'inexact->exact flonums-package))
   
   )     ; end of letrec bindings
  
  (set! /
        (letrec ((/2 (lambda (x y)
                       (cond ((and (exact? x) (exact? y))
                              (let ((z (quotient x y)))
                                (if (=? x (* y z))
                                    z
                                    (flo/ (number->flonum x) (number->flonum y)))))
                             ((and (number? x) (number? y))
                              (flo/ (number->flonum x) (number->flonum y)))
                             (else ((begin (cerror "Non-numeric argument to /" x y) /)
                                    x y))))))
          (lambda (x . args)
            (cond ((null? args) (/2 1 x))
                  ((null? (cdr args)) (/2 x (car args)))
                  (else (apply / (cons (/2 x (car args)) (cdr args))))))))
  
  ; Several of the predicates are now byte codes, so they have been commented out.
  
  ; Redefine integer? so it knows about bignums and inexact integers.
  
  ;(set! integer?
  ;  (lambda (x)
  ;    (if (fixnum? x)
  ;        #!true
  ;        (bignum? x))))
  
  ;(set! integer?
  ;      (lambda (x)
  ;        (cond ((fixnum? x) #t)
  ;              ((bignum? x) #t)
  ;              ((flonum? x) (= x (round x)))
  ;              (else #f))))
  ;
  ;(set! rational? integer?)
  ;
  ;(set! real?
  ;  (lambda (x)
  ;    (if (rational? x)
  ;        #!true
  ;        (flonum? x))))
  ;
  ;(set! complex? real?)
  
  ; (set! number? complex?)
  
  (set! even?
        (lambda (x)
          (if (fixnum? x)
              (zero? (logand x 1))
              (zero? (remainder x 2)))))
  
  (set! odd?
        (lambda (x)
          (not (even? x))))
  
  ;  (set! exact? (lambda (x) #!false))
  
  ;(set! exact? (lambda (x) (and (number? x) (not (flonum? x)))))
  ;
  ;(set! inexact? (lambda (x) (and (number? x) (flonum? x))))
  
  ; (set! eqv?
  ;   (lambda (x y)
  ;     (cond ((and (number? x) (number? y)) (=? x y))
  ;           (else (eq? x y)))))
  
  (set! memv
        (letrec ((memv (lambda (x y)
                         (cond ((null? y) #!false)
                               ((eqv? x (car y)) y)
                               (else (memv x (cdr y)))))))
          (lambda (x y)
            (if (symbol? x)
                (memq x y)
                (memv x y)))))
  
  (set! assv
        (letrec ((assv (lambda (x y)
                         (cond ((null? y) #!false)
                               ((eqv? x (caar y)) (car y))
                               (else (assv x (cdr y)))))))
          (lambda (x y)
            (if (symbol? x)
                (assq x y)
                (assv x y)))))
  
  ;(set! exact->inexact (lambda (n) (+ n flo-zero)))
  
  ;(set! inexact->exact number->exact)
  
  ; generic quotient
  ; magically makes remainder, modulo, gcd, lcm, and random be generic also!
  
  (letrec ((h9 (vector-ref **error-code-table** 9))
           (h10 (vector-ref **error-code-table** 10))
           (generic-quotient
            (lambda (x y)
              (exact->inexact
               (quotient (inexact->exact x)
                         (inexact->exact y)))))
           (make-handler
            (lambda (old-handler)
              (lambda (errcode bytecode machine-state)
                (let ((stk (vector-ref machine-state 0)))
                  (cond ((and (integer? (car stk)) (integer? (cadr stk)))
                         (set! stk (cons (generic-quotient (car stk) (cadr stk)) (cddr stk)))
                         (vector-set! machine-state 0 stk)
                         (vector-set! machine-state 1 (1+ (vector-ref machine-state 1)))
                         (restart-machine-state machine-state))
                        (else (old-handler errcode bytecode machine-state))))))))
    (vector-set! **error-code-table** 9 (make-handler h9))
    (vector-set! **error-code-table** 10 (make-handler h10))
    #t)
  
  "Generic arithmetic package Version 0"
  
  )      ; end of big letrec
 
 


;(define truncate         (access 'truncate flonums-package))
;(define round            (access 'round flonums-package))
(define floor            (access 'floor flonums-package))
(define ceiling          (access 'ceiling flonums-package))
 
(define exp)
(define log)
(define sqrt)
(define sin)
(define cos)
(define tan)
(define asin)
(define acos)
(define atan)
 
(letrec
  (
   (number->flonum     (access 'number->flonum flonums-package))
   (flo/               (access 'flo/ flonums-package))
   (flo-exp            (access 'flo-exp flonums-package))
   (flo-log            (access 'flo-log flonums-package))
   (flo-expt           (access 'flo-expt flonums-package))
   (flo-sqrt           (access 'flo-sqrt flonums-package))
   (flo-sin            (access 'flo-sin flonums-package))
   (flo-cos            (access 'flo-cos flonums-package))
   (flo-tan            (access 'flo-tan flonums-package))
   (flo-asin           (access 'flo-asin flonums-package))
   (flo-acos           (access 'flo-acos flonums-package))
   (flo-atan           (access 'flo-atan flonums-package))
 
   (domain-error
     (lambda (name function arg)
       ((begin (cerror
                 (string-append "Domain error in argument to " name)
                 arg)
               function)
        arg)))
 
  )
 
  (set! exp
        (lambda (x)
          (if (eq? x 0)
              1
              (flo-exp (number->flonum x)))))
 
  (set! log
        (lambda (x)
          (if (eq? x 1)
              0
              (if (positive? x)
                  (flo-log (number->flonum x))
                  (domain-error "log" log x)))))
 
  (set! expt
        (let ((real$expt
               (lambda (x y)
                 (flo-expt (number->flonum x)
                           (number->flonum y)))))
          (define (expt z1 z2)
            (cond ((eq? z2 0) 1)
                  ((exact? z2)
                   (if (negative? z2)
                       (/ 1 (z^k z1 (- z2)))
                       (z^k z1 z2)))
                  ((and (real? z1) (real? z2))
                   (real$expt z1 z2))
                  (else (domain-error "expt" expt (if (real? z1) z2 z1)))))
          (define (z^k z k)
            (cond ((zero? k) 1)
                  ((odd? k) (* z (z^k z (- k 1))))
                  (else (let ((z^k/2 (z^k z (quotient k 2))))
                          (* z^k/2 z^k/2)))))
          expt))
 
  (set! sqrt
        (let ((sqrt (lambda (x)
                      (if (negative? x)
                          (domain-error "sqrt" sqrt x)
                          (flo-sqrt (number->flonum x))))))
          (define (newton x c)
            (let ((xnew (quotient (+ (* x x) c) (+ x x))))
              (if (<= x xnew)
                  x
                  (newton xnew c))))
          (lambda (z)
            (cond ((eq? z 0) 0)
                  ((and (fixnum? z) (positive? z))
                   (let ((root (sqrt z)))
                     (if (and (= z (* root root))
                              (integer? root))
                         (inexact->exact root)
                         root)))
                  ((and (exact? z)
                        (integer? z)
                        (positive? z))
                   (let ((k (newton z z)))
                     (if (= z (* k k))
                         k
                         (sqrt z))))
                  (else (sqrt z))))))
 
  (set! sin
        (lambda (x)
          (if (eq? x 0)
              0
              (flo-sin (number->flonum x)))))
 
  (set! cos
        (lambda (x)
          (if (eq? x 0)
              1
              (flo-cos (number->flonum x)))))
 
  (set! tan
        (lambda (x)
          (if (eq? x 0)
              0
              (flo-tan (number->flonum x)))))
 
  (set! asin
        (lambda (x)
          (if (eq? x 0)
              0
              (flo-asin (number->flonum x)))))
 
  (set! acos
        (lambda (x)
          (if (eq? x 1)
              0
              (flo-acos (number->flonum x)))))
 
  (set! atan
        (lambda (x . y)
          (begin (set! y (if (not (null? y)) (car y) 1))
                 (flo-atan (number->flonum x) (number->flonum y)))))
 
  "Transcendentals Version 0.0"
)

(define (numerator x)
  (define (loop x y)
    (if (integer? x)
        (quotient x (gcd x y))
        (loop (* 2 x) (* 2 y))))
  (cond ((exact? x) x)
        ((rational? x) (loop x 1))
        ; most compact way to create the appropriate type error
        (else (< x 0))))

(define (denominator x)
  (define (loop x y)
    (if (integer? x)
        (quotient y (gcd x y))
        (loop (* 2 x) (* 2 y))))
  (cond ((exact? x) 1)
        ((rational? x) (loop x 1))
        (else (< x 0))))

; This code was written by Alan Bawden.
; Its copyright status is unknown to me.

(define (rationalize x e)
  (optimize space)
  (define (simplest-rational x y)
    (define (simplest-rational-internal x y)      ; assumes 0 < X < Y
      (let ((fx (floor x))        ; [X] <= X < [X]+1
            (fy (floor y)))       ; [Y] <= Y < [Y]+1, also [X] <= [Y]
        (cond ((not (< fx x))
               ;; X is an integer so X is the answer:
               fx)
              ((= fx fy)
               ;; [Y] = [X] < X < Y so expand the next term in the continued
               ;; fraction:
               (+ fx (/ (simplest-rational-internal (/ (- y fy)) (/ (- x fx))))))
              (else
               ;; [X] < X < [X]+1 <= [Y] <= Y so [X]+1 is the answer:
               (+ 1 fx)))))
    (cond ((< y x)
           ;; Y < X so swap and try again:
           (simplest-rational y x))
          ((not (< x y))
           ;; X = Y so if either is a rational that is the answer, otherwise
           ;; I don't know of anything implementation independent we can do.
           (cond ((rational? x) x)
                 ((rational? y) y)
                 (else (error "What should we do in this case?  [~S, ~S]" x y))))
          ((positive? x) 
           ;; 0 < X < Y which is what SIMPLEST-RATIONAL-INTERNAL expects:
           (simplest-rational-internal x y))
          ((negative? y)
           ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
           (- (simplest-rational-internal (- y) (- x))))
          (else
           ;; X <= 0 <= Y so zero is the answer:
           0)))
  (simplest-rational (- x e) (+ x e)))

(define flonums-package)
(define access)
 
(import '(/ integer? rational? real? complex? number?
          even? odd? exact? inexact?
          eqv? memv assv
          truncate round floor ceiling
          exp log expt sqrt sin cos tan asin acos atan))
