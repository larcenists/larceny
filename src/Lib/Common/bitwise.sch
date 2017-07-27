; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny -- R6RS procedures from (rnrs arithmetic bitwise).
; See also Lib/Arch/*/primops.sch and Compiler/common.imp.sch.

($$trace "bitwise")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bitwise-complain x name)
  (assertion-violation name "illegal argument" x))

; This modulus must be a power of two that is less than or equal
; to (greatest-fixnum) on all Larceny systems.

(define bitwise:lg-modulus 24)
(define bitwise:modulus (expt 2 bitwise:lg-modulus))

; Returns the (non-negative) low-order bits of an exact integer.

(define (bitwise:low k) (mod k bitwise:modulus))

; Returns the (possibly negative) high-order bits of an exact integer.
; FIXME: functions that call this repeatedly are slow.

(define (bitwise:high k) (div k bitwise:modulus))

; Combines high-order and low-order bits.

(define (bitwise:combine hi lo)
  (+ (* hi bitwise:modulus) lo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Exported procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bitwise-not x)
  (cond ((fixnum? x)
         (fxnot x))
        ((bignum? x)
         (- (- x) 1))
        (else bitwise:complain x 'bitwise-not)))

(define (bitwise-and . args)
  (define (bitwise-and2 x y)
    (cond ((and (fixnum? x) (fixnum? y))
           (fxand x y))
          ((= x 0) 0)
          ((= y 0) 0)
          ((= x -1) y)
          ((= y -1) x)
          (else
           (integer-logand x y))))
  (cond ((null? args) -1)
        ((null? (cdr args)) (car args))
        ((null? (cddr args))
         (bitwise-and2 (car args) (cadr args)))
        (else
         (apply bitwise-and
                (bitwise-and2 (car args) (cadr args))
                (cddr args)))))

(define (bitwise-ior . args)
  (define (bitwise-ior2 x y)
    (cond ((and (fixnum? x) (fixnum? y))
           (fxior x y))
          ((= x 0) y)
          ((= y 0) x)
          ((= x -1) -1)
          ((= y -1) -1)
          (else
           (integer-logior x y))))
  (cond ((null? args) 0)
        ((null? (cdr args)) (car args))
        ((null? (cddr args))
         (bitwise-ior2 (car args) (cadr args)))
        (else
         (apply bitwise-ior
                (bitwise-ior2 (car args) (cadr args))
                (cddr args)))))

(define (bitwise-xor . args)
  (define (bitwise-xor2 x y)
    (cond ((and (fixnum? x) (fixnum? y))
           (fxxor x y))
          ((= x 0) y)
          ((= y 0) x)
          ((= x y) 0)
          (else
           (integer-logxor x y))))
  (cond ((null? args) 0)
        ((null? (cdr args)) (car args))
        ((null? (cddr args))
         (bitwise-xor2 (car args) (cadr args)))
        (else
         (apply bitwise-xor
                (bitwise-xor2 (car args) (cadr args))
                (cddr args)))))

(define (bitwise-if x y z)
  (bitwise-ior (bitwise-and x y)
               (bitwise-and (bitwise-not x) z)))

(define (bitwise-bit-count x)
  (cond ((< x 0)
         (bitwise-not (bitwise-bit-count (bitwise-not x))))
        ((fixnum? x)
         (fxbit-count x))
        (else
         (+ (bitwise-bit-count (bitwise:high x))
            (fxbit-count (bitwise:low x))))))

(define (bitwise-length x)
  (cond ((fixnum? x)
         (fxlength x))
        ((bignum? x)
         (bitwise-length:bignum x))
        (else (bitwise-complain x 'bitwise-length))))

(define (bitwise-first-bit-set x)
  (if (fixnum? x)
      (fxfirst-bit-set x)
      (let* ((y (bitwise:low x))
             (z (fxfirst-bit-set y)))
        (if (>= z 0)
            z
            (+ bitwise:lg-modulus
               (bitwise-first-bit-set (bitwise:high x)))))))

(define (bitwise-bit-set? x y)
  (assert (>= y 0))
  (not (zero?
        (bitwise-and
         (bitwise-arithmetic-shift-left 1 y)
         x))))

(define (bitwise-copy-bit x y z)
  (assert (>= y 0))
  (assert (fx<=? 0 z 1))
  (let* ((mask (bitwise-arithmetic-shift-left 1 y)))
    (bitwise-if mask
                (bitwise-arithmetic-shift-left z y)
                x)))

;;; FIXME: this is really slow

(define (bitwise-bit-field x y z)
  (assert (>= y 0))
  (assert (>= z 0))
  (assert (<= y z))
  (let ((mask
         (bitwise-not
          (bitwise-arithmetic-shift-left -1 z))))
    (bitwise-arithmetic-shift-right
     (bitwise-and x mask)
     y)))

(define (bitwise-copy-bit-field to start end from)
  (assert (>= start 0))
  (assert (>= end 0))
  (assert (<= start end))
  (let* ((mask1
          (bitwise-arithmetic-shift-left -1 start))
         (mask2
          (bitwise-not
           (bitwise-arithmetic-shift-left -1 end)))
         (mask (bitwise-and mask1 mask2)))
    (bitwise-if mask
                (bitwise-arithmetic-shift-left from start)
                to)))

(define (bitwise-arithmetic-shift x y)
  ;; returns  (floor (* x (expt 2 y)))
  (if (< y 0)
      (bitwise-arithmetic-shift-right x (- y))
      (bitwise-arithmetic-shift-left  x y)))

(define (bitwise-arithmetic-shift-left x y)
  (assert (<= 0 y))
  (cond ((= y 0)
         x)
        ((fixnum? x)
         (* x (expt 2 y)))
        ((and (bignum? x)
              (fixnum? y))
         (let* ((n (bignum-length x))
                (result (bignum-alloc (+ 2 n (quotient y bits-per-bigit)))))
           (bignum-shift-left! x result y)
           (let ((result (big-normalize! result)))
             (if (< x 0)
                 (- result)
                 result))))
        (else
         (assertion-violation 'bitwise-arithmetic-shift-left
                              (errmsg 'msg:notexactintegers)
                              x y))))

(define (bitwise-arithmetic-shift-right x y)
  (define (accelerate n)
    (bitwise-arithmetic-shift-right (div x (expt 2 n)) (- y n)))
  (assert (<= 0 y))
  (cond ((< (bitwise-length x) y)
         (if (< x 0)
             -1
             0))
        ((fixnum? x)
         (div x (expt 2 y)))
        ((and (bignum? x)
              (fixnum? y))
         (let* ((n (bignum-length x))
                ;; FIXME: not sure why the (+ n 2) can't be n,
                ;; but the bignum code throws exceptions if it's just n
                (n (max 2 (- (+ n 2) (quotient y bits-per-bigit))))
                (result (bignum-alloc n)))
           (bignum-shift-right! x result y)
           (let ((result (big-normalize! result)))
             (if (< x 0)
                 (if (= x (- (* result (expt 2 y))))
                     (- result)
                     (- (+ result 1)))
                 result))))
        ;; FIXME: the following code should never be executed
        ((= x 0) x)
        ((= x -1) x)
        ((> y 256)
         (accelerate 256))
        ((> y 16)
         (accelerate 16))
        ((> y 2)
         (accelerate 2))
        (else
         (div x (expt 2 y)))))

(define (bitwise-rotate-bit-field n start end count)
  (assert (>= start 0))
  (assert (>= end 0))
  (assert (>= count 0))
  (assert (<= start end))
  (let ((width (- end start)))
    (if (positive? width)
      (let* ((count (mod count width))
             (field0
              (bitwise-bit-field n start end))
             (field1 (bitwise-arithmetic-shift-left
                      field0 count))
             (field2 (bitwise-arithmetic-shift-right
                      field0
                      (- width count)))
             (field (bitwise-ior field1 field2)))
        (bitwise-copy-bit-field n start end field))
      n)))

(define (bitwise-reverse-bit-field x y z)
  (define (loop field n result)
    (if (zero? n)
        result
        (loop (div field 2)
              (- n 1)
              (+ (mod field 2)
                 result
                 result))))
  (assert (<= 0 y))
  (assert (<= 0 z))
  (assert (<= y z))
  (let* ((field (bitwise-bit-field x y z))
         (rfield (if (and (fixnum? field)
                          (< (- z y) (fixnum-width)))
                     (fxreverse-bit-field field 0 (- z y))
                     (loop field (- z y) 0))))
    (bitwise-copy-bit-field x y z rfield)))
