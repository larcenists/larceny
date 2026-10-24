; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Test cases for fixnum-specific primitives.  

(define (run-fixnum-tests . rest)
  (display "Fixnums") (newline)
  (fixnum-test-functionality)
  (quotient-remainder-tests)
  (fxarithmetic-shift-right-tests)
  (fxarithmetic-shift-left-tests)
  (fxarithmetic-shift-tests)
  (if (null? rest)
      (fixnum-test-error)
      #t)
  )

; fxarithmetic-shift-right, compiled in line (for constant counts,
; without a range check), against floor division by powers of 2.

(define (fxarithmetic-shift-right-tests)
  (let* ((mpf (most-positive-fixnum))
         (mnf (most-negative-fixnum))
         (width (fixnum-width))
         (values (list 0 1 -1 5 -5 1000003 -1000003 mpf mnf)))
    (define (shift x k) (fxarithmetic-shift-right x k))   ; variable count
    (define (expected x k) (floor (/ x (expt 2 k))))
    (allof "fxarithmetic-shift-right"
     (allof-map "variable counts"
      (lambda (x)
        (do ((k 0 (+ k 1)))
            ((= k width))
          (test (string-append (number->string x) " by " (number->string k))
                (shift x k) (expected x k))))
      values)
     (allof-map "constant counts"
      (lambda (x)
        (test "by 0" (fxarithmetic-shift-right x 0) x)
        (test "by 2" (fxarithmetic-shift-right x 2) (expected x 2))
        (test "by 7" (fxarithmetic-shift-right x 7) (expected x 7))
        (test "by 61" (fxarithmetic-shift-right x 61) (expected x 61)))
      values)
     (mustfail "count = width" shift 5 width)
     (mustfail "count < 0" shift 5 -1)
     (mustfail "count not a fixnum" shift 5 (+ mpf 1))
     (mustfail "not a fixnum" shift (+ mpf 1) 2)
     (mustfail "not a fixnum, constant count"
               (lambda (x) (fxarithmetic-shift-right x 2)) 2.5))))

; fxarithmetic-shift-left, compiled in line, against multiplication by
; powers of 2: a result that is not a fixnum raises an implementation
; restriction, a count out of range or an operand that is not a fixnum
; an assertion violation.

(define (fxarithmetic-shift-left-tests)
  (let* ((mpf (most-positive-fixnum))
         (mnf (most-negative-fixnum))
         (width (fixnum-width))
         (values (list 0 1 -1 5 -5 1000003 -1000003 mpf mnf
                       (quotient mpf 2) (quotient mnf 2)
                       (+ (quotient mpf 2) 1) (- (quotient mnf 2) 1))))
    (define (shift x k) (fxarithmetic-shift-left x k))    ; variable count
    (define (expected x k)
      (let ((r (* x (expt 2 k))))
        (if (and (<= mnf r) (<= r mpf)) r 'restriction)))
    (define (kind thunk)
      (guard (c ((implementation-restriction-violation? c) 'restriction)
                ((assertion-violation? c) 'assertion)
                (else 'other))
        (thunk)))
    (allof "fxarithmetic-shift-left"
     (allof-map "variable counts"
      (lambda (x)
        (do ((k 0 (+ k 1)))
            ((= k width))
          (test (string-append (number->string x) " by " (number->string k))
                (kind (lambda () (shift x k)))
                (expected x k))))
      values)
     (allof-map "constant counts"
      (lambda (x)
        (test "by 0" (kind (lambda () (fxarithmetic-shift-left x 0)))
              (expected x 0))
        (test "by 1" (kind (lambda () (fxarithmetic-shift-left x 1)))
              (expected x 1))
        (test "by 7" (kind (lambda () (fxarithmetic-shift-left x 7)))
              (expected x 7))
        (test "by 61" (kind (lambda () (fxarithmetic-shift-left x 61)))
              (expected x 61)))
      values)
     (test "count = width" (kind (lambda () (shift 5 width))) 'assertion)
     (test "count < 0" (kind (lambda () (shift 5 -1))) 'assertion)
     (test "count not a fixnum" (kind (lambda () (shift 5 (+ mpf 1))))
           'assertion)
     (test "not a fixnum" (kind (lambda () (shift (+ mpf 1) 2))) 'assertion)
     (test "not a fixnum, constant count"
           (kind (lambda () (fxarithmetic-shift-left 2.5 2)))
           'assertion))))

; fxarithmetic-shift, compiled in line: left for counts >= 0, right
; for negative ones.

(define (fxarithmetic-shift-tests)
  (let* ((mpf (most-positive-fixnum))
         (mnf (most-negative-fixnum))
         (width (fixnum-width))
         (values (list 0 1 -1 5 -5 1000003 -1000003 mpf mnf
                       (quotient mpf 2) (quotient mnf 2))))
    (define (shift x k) (fxarithmetic-shift x k))         ; variable count
    (define (expected x k)
      (let ((r (if (>= k 0) (* x (expt 2 k)) (floor (/ x (expt 2 (- k)))))))
        (if (and (<= mnf r) (<= r mpf)) r 'restriction)))
    (define (kind thunk)
      (guard (c ((implementation-restriction-violation? c) 'restriction)
                ((assertion-violation? c) 'assertion)
                (else 'other))
        (thunk)))
    (define (error? thunk)
      (if (memq (kind thunk) '(restriction assertion)) #t #f))
    (allof "fxarithmetic-shift"
     (allof-map "variable counts"
      (lambda (x)
        (do ((k (- 1 width) (+ k 1)))
            ((= k width))
          (test (string-append (number->string x) " by " (number->string k))
                (kind (lambda () (shift x k)))
                (expected x k))))
      values)
     (allof-map "constant counts"
      (lambda (x)
        (test "by 3" (kind (lambda () (fxarithmetic-shift x 3)))
              (expected x 3))
        (test "by -3" (kind (lambda () (fxarithmetic-shift x -3)))
              (expected x -3)))
      values)
     (test "count = width" (error? (lambda () (shift 5 width))) #t)
     (test "count = -width" (error? (lambda () (shift 5 (- width)))) #t)
     (test "count = most negative fixnum" (error? (lambda () (shift 5 mnf)))
           #t)
     (test "count not a fixnum" (error? (lambda () (shift 5 (+ mpf 1)))) #t)
     (test "not a fixnum" (error? (lambda () (shift 2.5 2))) #t)
     (test "not a fixnum, right" (error? (lambda () (shift 2.5 -2))) #t))))

; Generic quotient and remainder, compiled in line for two fixnums on
; some targets: signs, small divisors, the ends of the fixnum range
; (the most negative fixnum divided by -1 is a bignum), bignum and
; flonum operands, and division by zero.  The expected values come
; from truncate on exact rationals.

(define (quotient-remainder-tests)
  (let* ((mpf (most-positive-fixnum))
         (mnf (most-negative-fixnum))
         (values (list 0 1 -1 2 -2 7 -7 12 -12 1000003 -1000003
                       mpf (- mpf 1) mnf (+ mnf 1) (quotient mpf 3)))
         (divisors (list 1 -1 2 -2 3 -3 7 -7 1000003 mpf mnf -4
                         (+ mpf 1) (- mnf 1)))) ; bignums
    (define (expected-q a b) (truncate (/ a b)))
    (define (expected-r a b) (- a (* b (truncate (/ a b)))))
    (allof "quotient and remainder"
     (allof-map "exact operands"
      (lambda (a)
        (for-each
         (lambda (b)
           (let ((name (string-append (number->string a) " by "
                                      (number->string b))))
             (test (string-append "quotient " name)
                   (quotient a b) (expected-q a b))
             (test (string-append "remainder " name)
                   (remainder a b) (expected-r a b))))
         divisors))
      values)
     (test "quotient of the most negative fixnum by -1"
           (quotient mnf -1) (- mnf))
     (test "remainder of the most negative fixnum by -1"
           (remainder mnf -1) 0)
     (test "quotient of flonums" (quotient 17. -5) -3.)
     (test "remainder of flonums" (remainder -17 5.) -2.)
     (mustfail "quotient by 0" (lambda (a b) (quotient a b)) 5 0)
     (mustfail "remainder by 0" (lambda (a b) (remainder a b)) 5 0)
     (mustfail "quotient of 0 by 0" (lambda (a b) (quotient a b)) 0 0))))

; The fixnum range depends on the word size (30-bit or 62-bit fixnums).

(define (fixnum-test-functionality)
  (let* ((mpf (most-positive-fixnum))
         (mnf (most-negative-fixnum))
         (x `((0
              0
              (#t #f #f 1 -1 0 0
               #t #f #t #f #t #t #f #t #f #t
               #t #f #t #f #t #t #f #t #f #t
               0 0 ,mpf ,mnf)
              (0 1 1 0 1 0 1 0 0 1 0 1 0
                     0 1 0 1 0 0 1 0 1 0))
             (-10
              10
              (#f #t #f -9 -11 0 -20
               #f #t #t #f #f #f #t #t #f #f
               #f #t #t #f #f #f #t #t #f #f
               -10 -100 ,mpf ,mnf)
              (1 0 1 1 0 0 1 1 1 0 0 1 1
                     1 0 0 1 1 1 0 0 1 1))
             (10
              -10
              (#f #f #t 11 9 0 20
               #f #f #f #t #t #f #f #f #t #t
               #f #f #f #t #t #f #f #f #t #t
               10 -100 ,mpf ,mnf)
              (1 1 0 1 1 1 0 0 1 1 1 0 0
                     1 1 1 0 0 1 1 1 0 0)))))
    (allof-map
     "Fixnum functionality." 
     (lambda (x)
       (let ((k (car x))
             (l (cadr x))
             (correct (cddr x)))
         (let ((res (fixnum-tester k l)))
           (if (not (equal? correct res))
               (begin (failure-message-failed (list "fixnum test" k l) 
                                              res 
                                              correct)
                      #f)
               #t))))
     x)))

(define (fixnum-tester k l)

  ; For result.

  (define (test1)
    (list (fxzero? k)
          (fxnegative? k)
          (fxpositive? k)
          (fx+ k 1)
          (fx- k 1)
          (fx+ k l)
          (fx- k l)
          (fx= k 0)
          (fx< k 0)
          (fx<= k 0)
          (fx> k 0)
          (fx>= k 0)
          (fx= k l)
          (fx< k l)
          (fx<= k l)
          (fx> k l)
          (fx>= k l)
          (fx=? k 0)
          (fx<? k 0)
          (fx<=? k 0)
          (fx>? k 0)
          (fx>=? k 0)
          (fx=? k l)
          (fx<? k l)
          (fx<=? k l)
          (fx>? k l)
          (fx>=? k l)
          (fx* k 1)
          (fx* k l)
          (most-positive-fixnum)
          (most-negative-fixnum)))

  ; For control.

  (define (test2)
    (list (if (fxzero? k) 0 1)
          (if (fxnegative? k) 0 1)
          (if (fxpositive? k) 0 1)
          (if (fx= k 0) 0 1)
          (if (fx< k 0) 0 1)
          (if (fx<= k 0) 0 1)
          (if (fx> k 0) 0 1)
          (if (fx>= k 0) 0 1)
          (if (fx= k l) 0 1)
          (if (fx< k l) 0 1)
          (if (fx<= k l) 0 1)
          (if (fx> k l) 0 1)
          (if (fx>= k l) 0 1)
          (if (fx=? k 0) 0 1)
          (if (fx<? k 0) 0 1)
          (if (fx<=? k 0) 0 1)
          (if (fx>? k 0) 0 1)
          (if (fx>=? k 0) 0 1)
          (if (fx=? k l) 0 1)
          (if (fx<? k l) 0 1)
          (if (fx<=? k l) 0 1)
          (if (fx>? k l) 0 1)
          (if (fx>=? k l) 0 1)))

  (list (test1) (test2)))

(define (fixnum-test-error)

  (define fxadd (lambda (a b) (fx+ a b)))
  (define fxsub (lambda (a b) (fx- a b)))
  (define fxmul (lambda (a b) (fx* a b)))
  (define fxneg (lambda (a) (fx-- a)))
  (define mpf (most-positive-fixnum))
  (define mnf (most-negative-fixnum))
  (define half (+ (quotient mpf 1024) 1))         ; half * 1024 > mpf
  
  (allof "Fixnum error cases"
   (mustfail "add#1" fxadd 10 1.5)                ; 1.5 is not a fixnum
   (mustfail "add#2" fxadd (- mpf 1000) 1001)     ; barely overflows (positive)
   (mustfail "add#3" fxadd (+ mnf 1000) -1001)    ; barely overflows (negative)

   (mustfail "sub#1" fxsub 10 1.5)                ; 1.5 is not a fixnum
   (mustfail "sub#2" fxsub (+ mnf 1000) 1001)     ; barely overflows (negative)
   (mustfail "sub#3" fxsub (- mpf 1000) -1001)    ; barely overflows (positive)

   (mustfail "neg#1" fxneg mnf)                   ; barely overflows (positive)
   (mustfail "neg#2" fxneg 1.5)                   ; 1.5 is not a fixnum

   (mustfail "mul#1" fxmul 10 1.5)                ; 1.5 is not a fixnum
   (mustfail "mul#2" fxmul half 1024)             ; barely overflows (positive)
   (mustfail "mul#3" fxmul half -1025)            ; overflows (negative)
   ))

; eof
