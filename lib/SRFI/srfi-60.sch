;;;; "logical.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define logical:base   (expt 2 29))

(define (logical:ash-4 x)
  (if (negative? x)
      (+ -1 (quotient (+ 1 x) 16))
      (quotient x 16)))

(define (logical:ash-29 x)
  (if (negative? x)
      (+ -1 (quotient (+ 1 x) logical:base))
      (quotient x logical:base)))

(define (logical:reduce op4 ident)
  (lambda args
    (do ((res ident (op4 res (car rgs) 1 0))
         (rgs args (cdr rgs)))
        ((null? rgs) res))))

;@
(define logand
  (letrec
      ((lgand
        (lambda (n2 n1 scl acc)
          (cond ((= n1 n2) (+ acc (* scl n1)))
                ((zero? n2) acc)
                ((zero? n1) acc)
                (else (lgand (logical:ash-29 n2)
                             (logical:ash-29 n1)
                             (* logical:base scl)
                             (+ (* (fxlogand (modulo n1 logical:base)
                                             (modulo n2 logical:base))
                                   scl)
                                acc)))))))
    (logical:reduce lgand -1)))
;@
(define logior
  (letrec
      ((lgior
        (lambda (n2 n1 scl acc)
          (cond ((= n1 n2) (+ acc (* scl n1)))
                ((zero? n2) (+ acc (* scl n1)))
                ((zero? n1) (+ acc (* scl n2)))
                (else (lgior (logical:ash-29 n2)
                             (logical:ash-29 n1)
                             (* logical:base scl)
                             (+ (* (fxlogior (modulo n1 logical:base)
                                             (modulo n2 logical:base))
                                   scl)
                                acc)))))))
    (logical:reduce lgior 0)))
;@
(define logxor
  (letrec
      ((lgxor
        (lambda (n2 n1 scl acc)
          (cond ((= n1 n2) acc)
                ((zero? n2) (+ acc (* scl n1)))
                ((zero? n1) (+ acc (* scl n2)))
                (else (lgxor (logical:ash-29 n2)
                             (logical:ash-29 n1)
                             (* logical:base scl)
                             (+ (* (fxlogxor (modulo n1 logical:base)
                                             (modulo n2 logical:base))
                                   scl)
                                acc)))))))
    (logical:reduce lgxor 0)))
;@
(define (lognot n) (- -1 n))
;@
(define (logtest n1 n2)
  (not (zero? (logand n1 n2))))
;@
(define (logbit? index n)
  (logtest (expt 2 index) n))
;@
(define (copy-bit index to bool)
  (if bool
      (logior to (arithmetic-shift 1 index))
      (logand to (lognot (arithmetic-shift 1 index)))))
;@
(define (bitwise-if mask n0 n1)
  (logior (logand mask n0)
          (logand (lognot mask) n1)))
;@
(define (bit-field n start end)
  (logand (lognot (ash -1 (- end start)))
          (arithmetic-shift n (- start))))
;@
(define (copy-bit-field to from start end)
  (bitwise-if (arithmetic-shift (lognot (ash -1 (- end start))) start)
              (arithmetic-shift from start)
              to))
;@
(define (rotate-bit-field n count start end)
  (define width (- end start))
  (set! count (modulo count width))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift
             (logior (logand mask (arithmetic-shift zn count))
                     (arithmetic-shift zn (- count width)))
             start)
            (logand (lognot (ash mask start)) n))))
;@
(define (arithmetic-shift n count)
  (if (negative? count)
      (let ((k (expt 2 (- count))))
        (if (negative? n)
            (+ -1 (quotient (+ 1 n) k))
            (quotient n k)))
      (* (expt 2 count) n)))
;@
(define integer-length
  (letrec ((intlen (lambda (n tot)
                     (case n
                       ((0 -1) (+ 0 tot))
                       ((1 -2) (+ 1 tot))
                       ((2 3 -3 -4) (+ 2 tot))
                       ((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
                       (else (intlen (logical:ash-4 n) (+ 4 tot)))))))
    (lambda (n) (intlen n 0))))
;@
(define logcount
  (letrec ((logcnt (lambda (n tot)
                     (if (zero? n)
                         tot
                         (logcnt (quotient n 16)
                                 (+ (vector-ref
                                     '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                     (modulo n 16))
                                    tot))))))
    (lambda (n)
      (cond ((negative? n) (logcnt (lognot n) 0))
            ((positive? n) (logcnt n 0))
            (else 0)))))
;@
(define (log2-binary-factors n)
  (+ -1 (integer-length (logand n (- n)))))

(define (bit-reverse k n)
  (do ((m (if (negative? n) (lognot n) n) (arithmetic-shift m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (logior (arithmetic-shift rvs 1) (logand 1 m))))
      ((negative? k) (if (negative? n) (lognot rvs) rvs))))
;@
(define (reverse-bit-field n start end)
  (define width (- end start))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift (bit-reverse width zn) start)
            (logand (lognot (ash mask start)) n))))
;@
(define (integer->list k . len)
  (if (null? len)
      (do ((k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((<= k 0) lst))
      (do ((idx (+ -1 (car len)) (+ -1 idx))
           (k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((negative? idx) lst))))
;@
(define (list->integer bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))
(define (booleans->integer . bools)
  (list->integer bools))

;;;;@ SRFI-60 aliases
(define ash arithmetic-shift)
(define bitwise-ior logior)
(define bitwise-xor logxor)
(define bitwise-and logand)
(define bitwise-not lognot)
(define bit-count logcount)
(define bit-set?   logbit?)
(define any-bits-set? logtest)
(define first-set-bit log2-binary-factors)
(define bitwise-merge bitwise-if)

;;; Legacy
;;(define (logical:rotate k count len) (rotate-bit-field k count 0 len))
;;(define (logical:ones deg) (lognot (ash -1 deg)))
;;(define integer-expt expt)            ; legacy name
