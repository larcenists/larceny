; Copyright 1999 William Clinger.
;
; $Id$
;
; Reasonably portable hashing on EQ?, EQV?, EQUAL?.
; Requires bignums, SYMBOL-HASH.
;
; Given any Scheme object, returns a non-negative exact integer
; less than 2^24.

(define object-hash (lambda (x) 0))    ; hash on EQ?, EQV?
(define equal-hash (lambda (x) 0))     ; hash on EQUAL?

(let ((n 16777216)
      (n-1 16777215)
      (adj:fixnum   9000000)
      (adj:negative 8000000)
      (adj:large    7900000)
      (adj:ratnum   7800000)
      (adj:complex  7700000)
      (adj:flonum   7000000)
      (adj:compnum  6900000)
      (adj:char     6111000)
      (adj:string   5022200)
      (adj:vector   4003330)
      (adj:misc     3000444)
      (adj:pair     2555000)
      (adj:proc     2321001)
      (adj:iport    2321002)
      (adj:oport    2321003)
      (adj:weird    2321004)
      (budget0      32))
  
  (define (combine hash adjustment)
    (modulo (+ hash hash hash adjustment) 16777216))
  
  (define (hash-on-equal x budget)
    (if (> budget 0)
        (cond ((string? x)
               (string-hash x))
              ((pair? x)
               (let ((budget (quotient budget 2)))
                 (combine (hash-on-equal (car x) budget)
                          (hash-on-equal (cdr x) budget))))
              ((vector? x)
               (let ((n (vector-length x))
                     (budget (quotient budget 4)))
                 (if (> n 0)
                     (combine
                      (combine (hash-on-equal (vector-ref x 0) budget)
                               (hash-on-equal (vector-ref x (- n 1)) budget))
                      (hash-on-equal (vector-ref x (quotient n 2))
                                     (+ budget budget)))
                     adj:vector)))
              (else
               (object-hash x)))
        adj:weird))
  
  (set! object-hash
        (lambda (x)
          (cond ((symbol? x)
                 (symbol-hash x))
                ((number? x)
                 (if (exact? x)
                     (cond ((integer? x)
                            (cond ((negative? x)
                                   (combine (object-hash (- x)) adj:negative))
                                  ((< x n)
                                   (combine x adj:fixnum))
                                  (else
                                   (combine (modulo x n) adj:large))))
                           ((rational? x)
                            (combine (combine (object-hash (numerator x))
                                              adj:ratnum)
                                     (object-hash (denominator x))))
                           ((real? x)
                            adj:weird)
                           ((complex? x)
                            (combine (combine (object-hash (real-part x))
                                              adj:complex)
                                     (object-hash (imag-part x))))
                           (else
                            adj:weird))
                     (cond (#t
                            ; We can't really do anything with inexact numbers
                            ; unless infinities and NaNs behave reasonably.
                            adj:flonum)
                           ((rational? x)
                            (combine
                             (combine (object-hash
                                       (inexact->exact (numerator x)))
                                      adj:flonum)
                             (object-hash (inexact->exact (denominator x)))))
                           ((real? x)
                            adj:weird)
                           ((complex? x)
                            (combine (combine (object-hash (real-part x))
                                              adj:compnum)
                                     (object-hash (imag-part x))))
                           (else adj:weird))))
                ((char? x)
                 (combine (char->integer x) adj:char))
                ((string? x)
                 (combine (string-length x) adj:string))
                ((vector? x)
                 (combine (vector-length x) adj:vector))
                ((eq? x #t)
                 (combine 1 adj:misc))
                ((eq? x #f)
                 (combine 2 adj:misc))
                ((null? x)
                 (combine 3 adj:misc))
                ((pair? x)
                 adj:pair)
                ((procedure? x)
                 adj:proc)
                ((input-port? x)
                 adj:iport)
                ((output-port? x)
                 adj:oport)
                (else
                 adj:weird))))
  
  (set! equal-hash
        (lambda (x)
          (hash-on-equal x budget0))))

; eof
