; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny -- R6RS procedures from (rnrs arithmetic fixnums).
; See also Lib/Arch/*/primops.sch and Compiler/common.imp.sch.

($$trace "fx")

; Argument checking.

(define (fx:check! name x)
  (if (not (fixnum? x))
      (assertion-violation name (errmsg 'msg:notfixnum) x)))

(define (fx:range-check! name x)
  (fx:check! name x)
  (if (not (and (fx<=? 0 x) (fx<? x (fixnum-width))))
      (assertion-violation name (errmsg 'msg:fixnumshift) x)))

(define (fx:check-result name x)
  (if (fixnum? x)
      x
      (raise-r6rs-exception (make-implementation-restriction-violation)
                            name
                            (errmsg 'msg:fixnumrange)
                            (list x))))

; fixnum? is a primop; see Lib/Arch/*/primops.sch

(define *fixnum-width* #f)
(define *least-fixnum* #f)
(define *greatest-fixnum* #f)

(define (fixnum-width)
  (let ((result *fixnum-width*))
    (if result
        result
        (begin (set! *fixnum-width*
                     (cdr (assq 'fixnum-bits (system-features))))
               (fixnum-width)))))

(define (least-fixnum)
  (or *least-fixnum*
      (let* ((w (fixnum-width))
             (w-2 (- w 2))
             (two^wm2 (expt 2 w-2)))
        (set! *least-fixnum* (- 0 two^wm2 two^wm2))
        (least-fixnum))))

(define (greatest-fixnum)
  (or *greatest-fixnum*
      (let* ((w (fixnum-width))
             (w-2 (- w 2))
             (two^wm2 (expt 2 w-2)))
        (set! *greatest-fixnum* (+ two^wm2 (- two^wm2 1)))
        (greatest-fixnum))))

;;; These can be slow because two- and three-argument
;;; cases are handled by Compiler/common.imp.sch
;;;
;;; FIXME: SRFI 143 generalized comparisons to allow 0 or more arguments.
;;; So far as I can tell, SRFI 143 fails to explain the semantics for
;;; 0 or 1 argument, saying they are "Semantically equivalent to" the
;;; generic comparison, which requires two or more arguments.  I'm
;;; going to assume SRFI 143 intended for all comparisons to require
;;; 2 or more arguments.

(define (fx=? x y . rest)
  (fx:check! 'fx=? x)
  (fx:check! 'fx=? y)
  (if (null? rest)
      (= x y)
      (and (= x y) (apply fx=? y rest))))

(define (fx<? x y . rest)
  (fx:check! 'fx<? x)
  (fx:check! 'fx<? y)
  (if (null? rest)
      (< x y)
      (and (< x y) (apply fx<? y rest))))

(define (fx>? x y . rest)
  (fx:check! 'fx>? x)
  (fx:check! 'fx>? y)
  (if (null? rest)
      (> x y)
      (and (> x y) (apply fx>? y rest))))

(define (fx<=? x y . rest)
  (fx:check! 'fx<=? x)
  (fx:check! 'fx<=? y)
  (if (null? rest)
      (<= x y)
      (and (<= x y) (apply fx<=? y rest))))

(define (fx>=? x y . rest)
  (fx:check! 'fx>=? x)
  (fx:check! 'fx>=? y)
  (if (null? rest)
      (>= x y)
      (and (>= x y) (apply fx>=? y rest))))

(define (fxzero? x) (fx=? x 0))
(define (fxpositive? x) (fx>? x 0))
(define (fxnegative? x) (fx<? x 0))

(define (fxodd? x) (not (fxzero? (fxand x 1))))
(define (fxeven? x) (fxzero? (fxand x 1)))

(define (fxmax x . rest)
  (define (loop x others)
    (if (null? others)
        x
        (let ((y (car others)))
          (if (fx<? x y)
              (loop y (cdr others))
              (loop x (cdr others))))))
  (fx:check! 'fxmax x)
  (loop x rest))

(define (fxmin x . rest)
  (define (loop x others)
    (if (null? others)
        x
        (let ((y (car others)))
          (if (fx>? x y)
              (loop y (cdr others))
              (loop x (cdr others))))))
  (fx:check! 'fxmin x)
  (loop x rest))

; These can be slow because the first-order case is handled
; by Compiler/common.imp.sch

(define (fx+ x y)
  (fx:check! 'fx+ x)
  (fx:check! 'fx+ y)
  (fx:check-result 'fx+ (+ x y)))

;;; FIXME: SRFI 143 restricted fx- to 2 arguments.  I'm ignoring that.

(define (fx- x . rest)
  (cond ((null? rest)
         (fx- 0 x))
        ((null? (cdr rest))
         (let ((y (car rest)))
           (fx:check! 'fx- x)
           (fx:check! 'fx- y)
           (fx:check-result 'fx- (- x y))))
        (assertion-violation 'fx- (errmsg 'msg:toomanyargs) (cons x rest))))

(define (fx* x y)
  (fx:check! 'fx* x)
  (fx:check! 'fx* y)
  (fx:check-result 'fx* (* x y)))

; This code is specialized from number.sch

(define (fxdiv-and-mod x y)
  (fx:check! 'fxdiv-and-mod x)
  (fx:check! 'fxdiv-and-mod y)
  (cond ((fx=? y 0)
         (assertion-violation 'fxdiv (errmsg 'msg:zerodivide) x y))
        ((fx>=? x 0)
         (values (quotient x y) (remainder x y)))
        ((fx<? y 0)
         ; x < 0, y < 0
         (let ((q (quotient x y)))
           (if (fixnum? q)
               (let ((r (fx- x (fx* q y))))
                 (if (fx=? r 0)
                     (values q 0)
                     (values (fx+ q 1) (fx- r y))))
               (fx:check-result 'fxdiv-and-mod q))))
        (else
         ; x < 0, y > 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               (values q 0)
               (values (fx- q 1) (fx+ r y)))))))

(define (fxdiv x y)
  (fx:check! 'fxdiv x)
  (fx:check! 'fxdiv y)
  (cond ((fx=? y 0)
         (assertion-violation 'fxdiv (errmsg 'msg:zerodivide) x y))
        ((fx>=? x 0)
         (quotient x y))
        ((fx<? y 0)
         ; x < 0, y < 0
         (let ((q (quotient x y)))
           (if (fixnum? q)
               (let ((r (fx- x (fx* q y))))
                 (if (fx=? r 0)
                     q
                     (fx+ q 1)))
               (fx:check-result 'fxdiv q))))
        (else
         ; x < 0, y > 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               q
               (fx- q 1))))))

(define (fxmod x y)
  (fx:check! 'fxdiv x)
  (fx:check! 'fxdiv y)
  (cond ((fx=? y 0)
         (assertion-violation 'fxmod (errmsg 'msg:zerodivide) x y))
        ((fx>=? x 0)
         (remainder x y))
        ((fx<? y 0)
         ; x < 0, y < 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               0
               (fx- r y))))
        (else
         ; x < 0, y > 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               0
               (fx+ r y))))))

(define (fxdiv0-and-mod0 x y)
  (call-with-values
   (lambda () (fxdiv-and-mod x y))
   (lambda (q r)
     (cond ((< r (abs (/ y 2)))
            (values q r))
           ((fx>? y 0)
            (values (fx+ q 1) (fx- x (fx* (fx+ q 1) y))))
           (else
            (values (fx- q 1) (fx- x (fx* (fx- q 1) y))))))))

(define (fxdiv0 x y)
  (call-with-values
   (lambda () (fxdiv0-and-mod0 x y))
   (lambda (q r) q)))

(define (fxmod0 x y)
  (call-with-values
   (lambda () (fxdiv0-and-mod0 x y))
   (lambda (q r) r)))

; FIXME:  These definitions are inefficient.

(define (fx+/carry fx1 fx2 fx3)
  (fx:check! 'fx+/carry fx1)
  (fx:check! 'fx+/carry fx2)
  (fx:check! 'fx+/carry fx3)
  (let* ((s (+ fx1 fx2 fx3))
         (s0 (mod0 s (expt 2 (fixnum-width))))
         (s1 (div0 s (expt 2 (fixnum-width)))))
    (values s0 s1)))

(define (fx-/carry fx1 fx2 fx3)
  (fx:check! 'fx-/carry fx1)
  (fx:check! 'fx-/carry fx2)
  (fx:check! 'fx-/carry fx3)
  (let* ((d (- fx1 fx2 fx3))
         (d0 (mod0 d (expt 2 (fixnum-width))))
         (d1 (div0 d (expt 2 (fixnum-width)))))
    (values d0 d1)))

(define (fx*/carry fx1 fx2 fx3)
  (fx:check! 'fx*/carry fx1)
  (fx:check! 'fx*/carry fx2)
  (fx:check! 'fx*/carry fx3)
  (let* ((s (+ (* fx1 fx2) fx3))
         (s0 (mod0 s (expt 2 (fixnum-width))))
         (s1 (div0 s (expt 2 (fixnum-width)))))
    (values s0 s1)))

; These can be slow because the usual cases are handled
; by Compiler/common.imp.sch

(define (fxnot x) (fxlognot x))

(define (fxand x . rest)
  (define (loop x others)
    (cond ((null? others)
           (fx:check! 'fxand x)
           x)
          ((null? (cdr others))
           (fxlogand x (car others)))
          (else
           (loop (fxlogand x (car others)) (cdr others)))))
  (loop x rest))

(define (fxior x . rest)
  (define (loop x others)
    (cond ((null? others)
           (fx:check! 'fxior x)
           x)
          ((null? (cdr others))
           (fxlogior x (car others)))
          (else
           (loop (fxlogior x (car others)) (cdr others)))))
  (loop x rest))

(define (fxxor x . rest)
  (define (loop x others)
    (cond ((null? others)
           (fx:check! 'fxxor x)
           x)
          ((null? (cdr others))
           (fxlogxor x (car others)))
          (else
           (loop (fxlogxor x (car others)) (cdr others)))))
  (loop x rest))

(define (fxif fx1 fx2 fx3)
  (fxior (fxand fx1 fx2)
         (fxand (fxnot fx1) fx3)))

; FIXME: The vector should be a bytevector, but that would
; cause a problem on host systems that don't have bytevectors.

(define (fxbit-count fx)
  (define (loop fx n)
    (if (fx=? fx 0)
        n
        (let* ((lobits (fxand fx #b00011111))
               (i (vector-ref
                   '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4
                      1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5)
                   lobits)))
          (loop (fxarithmetic-shift-right fx 5) (fx+ n i)))))
  (if (fx>=? fx 0)
      (loop fx 0)
      ;; R6RS errata says the ei of R6RS Libraries should be fx
      (fxnot (loop (fxnot fx) 0))))

(define (fxlength fx)
  (do ((result 0 (fx+ result 1))
       (bits (if (fx<? fx 0) (fxnot fx) fx)
             (fxarithmetic-shift-right bits 1)))
      ((fx=? bits 0)
       result)))

(define (fxfirst-bit-set fx)
  (if (fx=? fx 0)
      -1
      (do ((result 0 (fx+ result 1))
           (bits fx
                 (fxarithmetic-shift-right bits 1)))
          ((fx=? (fxand bits 1) 1)
           result))))

(define (fxbit-set? fx1 fx2)
  (fx:range-check! 'fxbit-set? fx2)
  (not (fxzero? (fxand fx1 (fxarithmetic-shift-left 1 fx2)))))

;;; FIXME: The SRFI 151 specification of fxcopy-bit makes the
;;; following claim:
;;;
;;;     Compatibility note: The R6RS analogue bitwise-copy-bit
;;;     as originally documented has a completely different
;;;     interface. (bitwise-copy-bit dest index source) replaces
;;;     the index'th bit of dest with the index'th bit of source.
;;;     It is equivalent to (bit-field-replace-same dest source
;;;     index (+ index 1)). However, an erratum made a silent
;;;     breaking change to interpret the third argument as 0 for
;;;     a false bit and 1 for a true bit. Some R6RS implementations
;;;     applied this erratum but others did not.
;;;
;;; Will is unable to find any trace of said erratum.  The R6RS
;;; errata do include a correction to the precondition, but do
;;; not include the "silent breaking change" mentioned above.
;;; Will also believes the above misstates the R6RS semantics
;;; of fxcopy-bit.

(define (fxcopy-bit fx1 fx2 fx3)
  (fx:range-check! 'fxcopy-bit fx2)
  (if (fx=? fx3 (fxand fx3 1))
      (let* ((mask (fxarithmetic-shift-left 1 fx2)))
        (fxif mask
              (fxarithmetic-shift-left fx3 fx2)
              fx1))
      (assertion-violation 'fxcopy-bit (errmsg 'msg:illegalarg3) fx3)))

(define (fxbit-field fx1 fx2 fx3)
  (fx:range-check! 'fxbit-field fx2)

  ;; FIXME: The R6RS Libraries document idiotically insists the third argument
  ;; of fxbit-field be less than (fixnum-width).  SRFI 143 does not (although
  ;; its author may have intended to add that restriction, which is a possible
  ;; interpretation of https://srfi-email.schemers.org/srfi-143/msg/5766955),
  ;; so the run-time check is enforced only in R6RS mode.

  (if (and (fx=? fx3 (fixnum-width))
           (eq? 'r6rs (larceny:execution-mode)))
      (assertion-violation 'fxbit-field
                           (errmsg 'msg:fixnumrange:idiotic-error)
                           fx3))
  (fx:range-check! 'fxbit-field fx3)
  (if (fx<=? fx2 fx3)
      (let* ((mask (fxnot (fxarithmetic-shift-left -1 fx3))))
        (fxarithmetic-shift-right (fxand fx1 mask) fx2))
      (assertion-violation 'fxbit-field (errmsg 'msg:rangeerror) fx1 fx2 fx3)))

(define (fxcopy-bit-field to start end from)
  (fx:range-check! 'fxcopy-bit-field start)
  (fx:range-check! 'fxcopy-bit-field end)
  (if (fx<=? start end)
      (let* ((mask1 (fxarithmetic-shift-left -1 start))
             (mask2 (fxnot
                     (fxarithmetic-shift-left -1 end)))
             (mask (fxand mask1 mask2)))
        (fxif mask
              (fxarithmetic-shift-left from start)
              to))
      (assertion-violation 'fxcopy-bit-field
                           (errmsg 'msg:rangeerror)
                           to start end from)))

(define (fxrotate-bit-field n start end count)
  (fx:range-check! 'fxrotate-bit-field start)
  (fx:range-check! 'fxrotate-bit-field end)
  (fx:range-check! 'fxrotate-bit-field count)
  (if (fx<=? start end)
      (let* ((width (fx- end start)))
        (if (fx=? width 0)
            n
            (let* ((count (fxmod count width))
                   (field0 (fxbit-field n start end))
                   (field1 (fxarithmetic-shift-left field0 count))
                   (field2 (fxarithmetic-shift-right field0 (fx- width count)))
                   (field (fxior field1 field2)))
              (fxcopy-bit-field n start end field))))
      (assertion-violation 'fxrotate-bit-field
                           (errmsg 'msg:rangeerror)
                           n start end count)))

(define (fxreverse-bit-field n start end)
  (fx:range-check! 'fxreverse-bit-field start)
  (fx:range-check! 'fxreverse-bit-field end)
  (if (fx<=? start end)
      (do ((width (fx- end start) (fx- width 1))
           (bits  (fxbit-field n start end)
                  (fxarithmetic-shift-right bits 1))
           (rbits 0
                  (fxior (fxarithmetic-shift-left rbits 1)
                         (fxand bits 1))))
          ((fx=? width 0)
           (fxcopy-bit-field n start end rbits)))
      (assertion-violation 'fxreverse-bit-field
                           (errmsg 'msg:rangeerror)
                           n start end)))

; Shift procedures.

(define (fxarithmetic-shift n shift)
  (if (fxnegative? shift)
      (fxarithmetic-shift-right n (fx- shift))
      (fxarithmetic-shift-left n shift)))

(define (fxarithmetic-shift-left n shift)
  (fx:check! 'fxarithmetic-shift-left n)
  (fx:range-check! 'fxarithmetic-shift-left shift)
  (let* ((result (fxlsh n shift))
         (ncheck (fxrsha result shift)))
    (if (fx=? n ncheck)
        result
        (raise-r6rs-exception (make-implementation-restriction-violation)
                              'fxarithmetic-shift-left
                              (errmsg 'msg:fixnumrange)
                              (list n shift)))))

(define (fxarithmetic-shift-right n shift)
  (fx:check! 'fxarithmetic-shift-right n)
  (fx:range-check! 'fxarithmetic-shift-right shift)
  (fxrsha n shift))

;;; Added for SRFI 143.

(define (fxneg x) (fx- 0 x))

(define (fxquotient x y)
  (fx:check! 'fxquotient x)
  (fx:check! 'fxquotient y)
  (fx:check-result 'fxquotient (quotient x y)))

(define (fxremainder x y)
  (fx:check! 'fxremainder x)
  (fx:check! 'fxremainder y)
  (fx:check-result 'fxremainder (remainder x y)))

(define (fxabs x)
  (fx:check! 'fxabs x)
  (fx:check-result 'fxabs (abs x)))

(define (fxsquare x)
  (fx* x x))

(define (fxsqrt x)
  (fx:check! 'fxsqrt x)
  (exact-integer-sqrt x))

(define (fxfirst-set-bit x)
  (fxfirst-bit-set x))

(define (fxbit-field-rotate i count start end)    ; note permutation of args
  (if (fxnegative? count)
      (fxrotate-bit-field i start end (+ count (- end start)))
      (fxrotate-bit-field i start end count)))

(define (fxbit-field-reverse x y z)
  (fxreverse-bit-field x y z))
