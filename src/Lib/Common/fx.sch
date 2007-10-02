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
      (assertion-violation name "argument not a fixnum" x)))

(define (fx:range-check! name x)
  (fx:check! name x)
  (if (not (and (fx<=? 0 x) (fx<? x (fixnum-width))))
      (assertion-violation name "fixnum shift count out of range" x)))

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

; These can be slow because two- and three-argument
; cases are handled by Compiler/common.imp.sch

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
  (let ((result (+ x y)))
    (if (fixnum? result)
        result
        (assertion-violation 'fx+ "result out of range" x y))))

(define (fx- x . rest)
  (cond ((null? rest)
         (fx- 0 x))
        ((null? (cdr rest))
         (let ((y (car rest)))
           (fx:check! 'fx- x)
           (fx:check! 'fx- y)
           (let ((result (- x y)))
             (if (fixnum? result)
                 result
                 (assertion-violation 'fx- "result out of range" x y)))))
        (assertion-violation 'fx- "too many arguments" (cons x rest))))

(define (fx* x y)
  (fx:check! 'fx* x)
  (fx:check! 'fx* y)
  (let ((result (* x y)))
    (if (fixnum? result)
        result
        (assertion-violation 'fx* "result out of range" x y))))

; This code is specialized from number.sch

(define (fxdiv-and-mod x y)
  (fx:check! 'fxdiv-and-mod x)
  (fx:check! 'fxdiv-and-mod y)
  (cond ((fx=? y 0)
         (assertion-violation 'fxdiv "zero divisor" x y))
        ((fx>=? x 0)
         (values (quotient x y) (remainder x y)))
        ((fx<? y 0)
         ; x < 0, y < 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               (values q 0)
               (values (fx+ q 1) (fx- r y)))))
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
         (assertion-violation 'fxdiv "zero divisor" x y))
        ((fx>=? x 0)
         (quotient x y))
        ((fx<? y 0)
         ; x < 0, y < 0
         (let* ((q (quotient x y))
                (r (fx- x (fx* q y))))
           (if (fx=? r 0)
               q
               (fx+ q 1))))
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
         (assertion-violation 'fxmod "zero divisor" x y))
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
      ; FIXME: the 5.97 draft contains a typo, so this is just a guess.
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

(define (fxcopy-bit fx1 fx2 fx3)
  (fx:range-check! 'fxcopy-bit fx2)
  (if (fx=? fx3 (fxand fx3 1))
      (let* ((mask (fxarithmetic-shift-left 1 fx2)))
        (fxif mask
              (fxarithmetic-shift-left fx3 fx2)
              fx1))
      (assertion-violation 'fxcopy-bit "illegal third argument" fx3)))

; FIXME: The 5.97 draft idiotically insists that the third argument
; of fxbit-field be less than (fixnum-width).

(define fixnum-range:idiotic-error
  "the R6RS requires the third argument to be less than (fixnum-width)")

(define (fxbit-field fx1 fx2 fx3)
  (fx:range-check! 'fxbit-field fx2)
  (if (fx=? fx3 (fixnum-width))
      (assertion-violation 'fxbit-field fixnum-range:idiotic-error fx3))
  (fx:range-check! 'fxbit-field fx3)
  (if (fx<=? fx2 fx3)
      (let* ((mask (fxnot (fxarithmetic-shift-left -1 fx3))))
        (fxarithmetic-shift-right (fxand fx1 mask) fx2))
      (assertion-violation 'fxbit-field "range error" fx1 fx2 fx3)))

(define (fxcopy-bit-field to start end from)
  (fx:range-check! 'fxcopy-bit-field start)
  (fx:range-check! 'fxcopy-bit-field end)
  (if (fx<=? fx2 fx3)
      (let* ((mask1 (fxarithmetic-shift-left -1 start))
             (mask2 (fxnot
                     (fxarithmetic-shift-left -1 end)))
             (mask (fxand mask1 mask2)))
        (fxif mask
              (fxarithmetic-shift-left from start)
              to))
      (assertion-violation 'fxcopy-bit-field "range error" to start end from)))

(define (fxrotate-bit-field n start end count)
  (fx:range-check! 'fxrotate-bit-field start)
  (fx:range-check! 'fxrotate-bit-field end)
  (fx:range-check! 'fxrotate-bit-field count)
  (if (and (fx<=? start end)
           (fx<=? width (fx- end start)))
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
                           "range error" n start end count)))

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
      (assertion-violation 'fxreverse-bit-field "range error" n start end)))

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
        (assertion-violation 'fxarithmetic-shift-left
                             "result not a fixnum" n shift))))

(define (fxarithmetic-shift-right n shift)
  (fx:check! 'fxarithmetic-shift-right n)
  (fx:range-check! 'fxarithmetic-shift-right shift)
  (fxrsha n shift))
