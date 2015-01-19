; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Predicates.
; Number predicates are tested by number.sch, but we still need to test 
; many other things in this file:
;  - pair?
;  - null?
;  - boolean?
;  - symbol?
;  - structure?
;  - port?, input-port?, output-port?
;  - list?
;  - vector?
;  - char?
;  - number?
;  - procedure?

(define (run-predicate-tests)
  (display "Predicate") (newline)
  (test-equality-primitives-helper 0 1 1 2 'a 'b)
  (predicate-test-0)
  (symbol-predicate-test))

(define (test-equality-primitives-helper zero one xone two a b)
  (allof
   (test "(eq? a 'a)" (eq? a 'a) #t)
   (test "(eq? a b)" (eq? a b) #f)
   (test "(eq? one xone)" (eq? one xone) #t)
   (test "(eq? zero one)" (eq? zero one) #f)
   (test "(eqv? a 'a)" (eqv? a 'a) #t)
   (test "(eqv? a b)" (eqv? a b) #f)
   (test "(eqv? one xone)" (eqv? one xone) #t)
   (test "(eqv? zero one)" (eqv? zero one) #f)
   (test "(equal? a 'a)" (equal? a 'a) #t)
   (test "(equal? a b)" (equal? a b) #f)
   (test "(equal? xone one)" (equal? xone one) #t)
   (test "(equal? zero one)" (equal? zero one) #f)
   ))

; Tests eq? and eqv?, first time around (basic stuff, no numbers.)

(define (predicate-test-0)

  (define (e a b)
    (list (eq? a b) (eqv? a b)))

  (let ((a "string1")
	(b (lambda (x) x)))
    (allof
     (test "(e '() '())" (e '() '()) '(#t #t))
     (test "(e (vector) (vector))" (e (vector) (vector)) '(#f #t))
     (test "(e (string) (string))" (e (string) (string)) '(#f #t))
     (test "(e #t #f)" (e #t #f) '(#f #f))
     (test "(e a a)" (e a a) '(#t #t))
     (test "(e b b)" (e b b) '(#t #t))
     (test "(e 'foo 'foo)" (e 'foo 'foo) '(#t #t))
     (test "(e (lambda () 1) (lambda () 1))"
           (e (lambda () 1) (lambda () 1))
           '(#f #f))
     (test "(e (closure 0) (closure 0))"
           (let ((closure (lambda (n) (lambda () n))))
             (e (closure 0) (closure 0)))
           '(#f #f))
     (test "(e (closure 1) (closure 1))"
           (let* ((n (- (expt 3 3) 27))
                  (closure (lambda (ignored) (lambda () n))))
             (e (closure 1) (closure 1)))
           '(#f #f))
     )))

; Tests symbol=?

(define (symbol-predicate-test)

  (allof
   (test "(symbol=? 'a 'a)" (symbol=? 'a 'a) #t)
   (test "(symbol=? 'a 'a 'a)" (symbol=? 'a 'a 'a) #t)
   (test "(symbol=? 'a 'a 'b)" (symbol=? 'a 'a 'b) #f)
   (test "(symbol=? 'a 'b)" (symbol=? 'a 'b) #f)
   (test "(symbol=? 'a 'b 'a)" (symbol=? 'a 'b 'a) #f)
   (test "(symbol=? 'a 'b 'b)" (symbol=? 'a 'b 'b) #f)
   ))

; eof
