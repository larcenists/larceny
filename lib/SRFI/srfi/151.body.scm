;;; SRFI 151 (bitwise operations)
;;;
;;; $Id$
;;;
;;; Copyright (C) William D Clinger (2017).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 


;;; "The bitwise-eqv procedure produces the complement of the
;;; bitwise-xor procedure."

(define (bitwise-eqv . args)
  (bitwise-not (apply bitwise-xor args)))

(define (bitwise-nand i j)
  (bitwise-not (bitwise-and i j)))

(define (bitwise-nor i j)
  (bitwise-not (bitwise-ior i j)))

(define (bitwise-andc1 i j)
  (bitwise-and (bitwise-not i) j))

(define (bitwise-andc2 i j)
  (bitwise-andc1 j i))

(define (bitwise-orc1 i j)
  (bitwise-ior (bitwise-not i) j))

(define (bitwise-orc2 i j)
  (bitwise-orc1 j i))


;;; R6RS bitwise-bit-set? uses the opposite order of arguments.

(define (bit-set? index i)
  (bitwise-bit-set? i index))

;;; R6RS bitwise-copy-bit uses the opposite order of the first two
;;; arguments, and takes a bit instead of a boolean as third argument.

(define (copy-bit index i b)
  (bitwise-copy-bit i index (if b 1 0)))

(define (bit-swap index1 index2 i)
  (bitwise-copy-bit (bitwise-copy-bit i
                                      index1
                                      (if (bitwise-bit-set? i index2)
                                          1
                                          0))
                    index2
                    (if (bitwise-bit-set? i index1)
                        1
                        0)))


(define (any-bit-set? test-bits i)
  (not (zero? (bitwise-and test-bits i))))

(define (every-bit-set? test-bits i)
  (= test-bits (bitwise-and test-bits i)))


(define (bit-field-any? i start end)
  (not (zero? (bit-field i start end))))

(define (bit-field-every? i start end)
  (= (- (expt 2 (- end start)) 1)
     (bit-field i start end)))

(define (bit-field-clear i start end)
  (bitwise-copy-bit-field i start end 0))

(define (bit-field-set i start end)
  (bitwise-copy-bit-field i start end -1))

(define (bit-field-replace dst src start end)
  (let* ((k (max 0 (- end start)))
         (mask (- (expt 2 k) 1))
         (field (bitwise-and mask src)))
    (bitwise-ior (bitwise-arithmetic-shift-left
                  (bitwise-arithmetic-shift-right dst end)
                  end)
                 (bitwise-arithmetic-shift-left field start)
                 (bit-field dst 0 start))))

(define (bit-field-replace-same dst src start end)
  (bitwise-copy-bit-field dst start end src))

;;; SRFI 151 allows the count to be negative.

(define (bit-field-rotate i count start end)
  (if (and (negative? count) (< start end))
      (bit-field-rotate i (+ count (- end start)) start end) ; FIXME
      (bitwise-rotate-bit-field i start end count)))


(define (bits->list i . rest)
  (vector->list (apply bits->vector i rest)))

(define (bits->vector i . rest)
  (if (and (exact-integer? i) (not (negative? i)))
      (let* ((len (if (null? rest) (integer-length i) (car rest)))
             (v   (make-vector len #f)))
        (let loop ((i i)
                   (k 0))
          (if (= k len)
              v
              (begin (if (odd? i)
                         (vector-set! v k #t))
                     (loop (quotient i 2) (+ k 1))))))
      (error 'bits->vector (larceny:errmsg 'msg:notnaturalnumber) i)))

(define (list->bits bools)
  (vector->bits (list->vector bools)))

(define (vector->bits v)
  (let ((len (vector-length v)))
    (let loop ((result 0)
               (k 0)
               (two^k 1))
      (if (= k len)
          result
          (loop (if (vector-ref v k) (+ result two^k) result)
                (+ k 1)
                (+ two^k two^k))))))

(define (bits . bools)
  (list->bits bools))

(define (bitwise-fold proc seed i)
  (fold-left (lambda (x y) (proc y x))
             seed
             (bits->list i)))

(define (bitwise-for-each proc i)
  (for-each proc (bits->list i)))

(define (bitwise-unfold stop? mapper successor seed)
  (define (loop seed two^k result)
    (if (stop? seed)
        result
        (loop (successor seed)
              (+ two^k two^k)
              (if (mapper seed)
                  (+ two^k result)
                  result))))
  (loop seed 1 0))

(define (make-bitwise-generator i)
  (lambda ()
    (let ((result (odd? i)))
      (set! i (quotient i 2))
      result)))
