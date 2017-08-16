; Copyright 1999 William Clinger.
;
; $Id$
;
; Hashing on EQ?, EQV?, EQUAL?.
; Requires bignums, SYMBOL-HASH.
; FIXME: Larceny-specific because of how records and procedures are hashed.
;
; Given any Scheme object, returns a non-negative exact integer
; less than 2^24.
;
; The equal? procedure doesn't look inside records,
; so equal-hash of a record can't depend on any mutable fields.

; FIXME: object-hash is not required by R5RS or R6RS, but
; Larceny's implementation of it should still be improved.

(define object-hash (lambda (x) 0))    ; hash on EQ?, EQV?

(define equal-hash (lambda (x) 0))     ; hash on EQUAL?

; string-hash and string-ci-hash are defined in Lib/Common/string.sch
; symbol-hash is defined in Lib/Common/oblist.sch

;; Hook for instances
(define procedure-hasher
  (make-parameter "procedure-hasher"
                  (lambda (procedure) 2321001)
                  procedure?))

(let ((n 16777216)
      (n-1 16777215)
      (adj:fixnum   9000000)
      (adj:negative 8000000)
      (adj:large    7900000)
      (adj:ratnum   7800000)
      (adj:complex  7700000)
      (adj:flonum   7000000)
      (adj:compnum  6900000)
      (adj:nan      6765432)
      (adj:neginf   6645789)
      (adj:posinf   6567123)
      (adj:zero     6476213)
      (adj:char     6111000)
      (adj:string   5022200)
      (adj:bvector  4488623)
      (adj:bvector0 3537986)
      (adj:bvector1 3194063)
      (adj:bvector2 3633447)
      (adj:bvector3  959672)
      (adj:vector   4003330)
      (adj:vector0  4749990)
      (adj:vector1  5127702)
      (adj:vector2  5061590)
      (adj:vector3  1429329)
      (adj:record   5196360)
      (adj:proc     4591091)
      (adj:misc     3000444)
      (adj:pair     2555000)
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
              ((bytevector? x)
               (let* ((n (bytevector-length x))
                      (limit (min n budget))
                      (i1 (quotient n 3))
                      (i2 (quotient (+ n n) 3)))
                 (if (> n 0)
                     (combine
                      (combine (do ((i 0 (+ i 1))
                                    (h adj:bvector0
                                       (combine h (bytevector-ref x i))))
                                   ((= i limit)
                                    h))
                               (bytevector-ref x i1))
                      (combine (bytevector-ref x i2)
                               (bytevector-ref x (- n 1))))
                     adj:bvector)))
              ((record? x)
               (combine (hash-on-equal (record-type-name (record-rtd x))
                                       budget)
                        adj:record))
              ((procedure? x)
               (combine (hash-on-equal (procedure-ref x 0) budget)
                        adj:proc))
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
                     (cond ((compnum? x)
                            (combine (combine (object-hash (real-part x))
                                              adj:compnum)
                                     (object-hash (imag-part x))))
                           ((nan? x)
                            adj:nan)
                           ((infinite? x)
                            (if (< x 0) adj:neginf adj:posinf))
                           ((= x 0.0)
                            adj:zero)
                           ((rational? x)
                            (combine
                             (combine (object-hash
                                       (inexact->exact (numerator x)))
                                      adj:flonum)
                             (object-hash (inexact->exact (denominator x)))))
                           ((real? x)
                            adj:weird)
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
                 ((procedure-hasher) x))
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
