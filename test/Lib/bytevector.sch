; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; $Id$

(define (run-bytevector-tests)
  (display "Bytevector") (newline)
  (basic-bytevector-tests)
  (ieee-bytevector-tests)

  ; There is little point to testing Unicode conversions
  ; if Unicode strings aren't supported in the system
  ; we're testing.

  (let* ((string-rep (cdr (assq 'string-representation (system-features))))
         (unicode? (not (eq? string-rep 'flat1))))
    (if unicode?
        (begin
         (string-bytevector-tests)
         (exhaustive-string-bytevector-tests)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The deprecated endianness syntax is not present in R5RS mode.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax endianness
  (syntax-rules ()
   ((endianness x)
    (quote x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of R6RS silliness.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (basic-bytevector-tests)
  (allof "basic bytevector tests"

   (test "endianness, big" (endianness big) 'big)
   (test "endianness, little" (endianness little) 'little)

   (test "native-endianness"
         (or (eq? (native-endianness) 'big)
             (eq? (native-endianness) 'little))
         #t)

   (test "not vector"
         (bytevector? (vector))
         #f)
   (test "make-bytevector" (bytevector? (make-bytevector 3)) #t)

   (test "bytevector-length" (bytevector-length (make-bytevector 44)) 44)

   (test "bytevector-x8-ref, part 1"
         (let ((b1 (make-bytevector 16 -127))
               (b2 (make-bytevector 16 255)))
           (list
            (bytevector-s8-ref b1 0)
            (bytevector-u8-ref b1 0)
            (bytevector-s8-ref b2 0)
            (bytevector-u8-ref b2 0)))
         '(-127 129 -1 255))

   (test "bytevector-x8-ref, part 2"
         (let ((b (make-bytevector 16 -127)))
           (bytevector-s8-set! b 0 -126)
           (bytevector-u8-set! b 1 246)
           (list
            (bytevector-s8-ref b 0)
            (bytevector-u8-ref b 0)
            (bytevector-s8-ref b 1)
            (bytevector-u8-ref b 1)))
         '(-126 130 -10 246))

   (let ()
     (define b (make-bytevector 16 -127))
     (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

     (test "bytevector-uint-ref"
           (bytevector-uint-ref b 0 (endianness little) 16)
           #xfffffffffffffffffffffffffffffffd)

     (test "bytevector-sint-ref"
           (bytevector-sint-ref b 0 (endianness little) 16)
           -3)

     (test "bytevector->u8-list"
           (bytevector->u8-list b)
           '(253 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 255))

     (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

     (test "bytevector-uint-set!, part 1"
           (bytevector-uint-ref b 0 (endianness big) 16)
           #xfffffffffffffffffffffffffffffffd)

     (test "bytevector-uint-set!, part 2"
           (bytevector-sint-ref b 0 (endianness big) 16)
           -3)

     (test "bytevector-uint-set!, part 3"
           (bytevector->u8-list b)
           '(255 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 253)))

   (let ()
     (define b
       (u8-list->bytevector
        '(255 255 255 255 255 255 255 255
          255 255 255 255 255 255 255 253)))

     (test "u16-ref" (bytevector-u16-ref b 14 (endianness little)) 65023)

     (test "s16-ref" (bytevector-s16-ref b 14 (endianness little)) -513)

     (test "u16-ref, big" (bytevector-u16-ref b 14 (endianness big)) 65533)

     (test "s16-ref, big" (bytevector-s16-ref b 14 (endianness big)) -3)

     (bytevector-u16-set! b 0 12345 (endianness little))

     (test "u16-set!" (bytevector-u16-ref b 0 (endianness little)) 12345)

     (bytevector-u16-native-set! b 0 12345)

     (test "u16-native" (bytevector-u16-native-ref b 0) 12345))

   (let ()
     (define b
       (u8-list->bytevector
       '(255 255 255 255 255 255 255 255
         255 255 255 255 255 255 255 253)))

     (test "u32-ref" (bytevector-u32-ref b 12 (endianness little)) 4261412863)

     (test "s32-ref" (bytevector-s32-ref b 12 (endianness little)) -33554433)

     (test "u32-ref, big"
           (bytevector-u32-ref b 12 (endianness big)) 4294967293)

     (test "s32-ref, big" (bytevector-s32-ref b 12 (endianness big)) -3))

   (let ()
     (define b
       (u8-list->bytevector
        '(255 255 255 255 255 255 255 255
          255 255 255 255 255 255 255 253)))

     (test "u64-ref" (bytevector-u64-ref b 8 (endianness little))
                     18302628885633695743)

     (test "s64-ref" (bytevector-s64-ref b 8 (endianness little))
                     -144115188075855873)

     (test "u64-ref, big" (bytevector-u64-ref b 8 (endianness big))
                          18446744073709551613)

     (test "s64-ref, big" (bytevector-s64-ref b 8 (endianness big)) -3))

   (let ()
     (define b1 (u8-list->bytevector '(255 2 254 3 255)))
     (define b2 (u8-list->bytevector '(255 3 254 2 255)))
     (define b3 (u8-list->bytevector '(255 3 254 2 255)))
     (define b4 (u8-list->bytevector '(255 3 255)))

     (test "bv=12" (bytevector=? b1 b2) #f)
     (test "bv=23" (bytevector=? b2 b3) #t)
     (test "bv=34" (bytevector=? b3 b4) #f)
     (test "bv=43" (bytevector=? b4 b3) #f))

   (let ()
     (define b
       (u8-list->bytevector
        '(63 240 0 0 0 0 0 0)))

     (test "single-ref" (bytevector-ieee-single-ref b 4 'little) 0.0)

     (test "double-ref" (bytevector-ieee-double-ref b 0 'big) 1.0)

     (bytevector-ieee-single-native-set! b 4 3.0)

     (test "single-native-ref"
           (bytevector-ieee-single-native-ref b 4) 3.0)

     (bytevector-ieee-double-native-set! b 0 5.0)

     (test "double-native-ref" (bytevector-ieee-double-native-ref b 0) 5.0)

     (bytevector-ieee-double-set! b 0 1.75 'big)

     (test "double-set!" (bytevector->u8-list b) '(63 252 0 0 0 0 0 0)))

   (let ((b (make-bytevector 7 12)))
     (bytevector-fill! b 127)

     (test "fill!" (bytevector->u8-list b) '(127 127 127 127 127 127 127)))

   (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
     (bytevector-copy! b 0 b 3 4)

     (test "copy!" (bytevector->u8-list b) '(1 2 3 1 2 3 4 8))
     (test "copy" (bytevector=? b (bytevector-copy b)) #t))

   (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
     (test "->sint-list" (bytevector->sint-list b (endianness little) 2)
                         '(513 -253 513 513))
     (test "->uint-list" (bytevector->uint-list b (endianness little) 2)
                         '(513 65283 513 513)))))


(define (ieee-bytevector-tests)

  (define (roundtrip x getter setter! k endness)
    (let ((b (make-bytevector 100)))
      (setter! b k x endness)
      (getter b k endness)))

  (define (->single x)
    (roundtrip
     x bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big))

  (define (->double x)
    (roundtrip
     x bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big))

  (define (tst1 actual expected)
    (let ((id (cond ((number? expected)
                     (number->string expected))
                    ((boolean? expected)
                     (if expected "#t" "#f"))
                    (else "ieee test"))))
      (set! results
            (cons (test id actual expected) results))))

  (define results '())

  ; Single precision, offset 0, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  0 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'big)
        -0.2822580337524414)

  ; Single precision, offset 0, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  0 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 0 'little)
        -0.2822580337524414)

  ; Single precision, offset 1, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  1 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'big)
        -0.2822580337524414)

  ; Single precision, offset 1, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  1 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 1 'little)
        -0.2822580337524414)

  ; Single precision, offset 2, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  2 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'big)
        -0.2822580337524414)

  ; Single precision, offset 2, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  2 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 2 'little)
        -0.2822580337524414)

  ; Single precision, offset 3, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  3 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'big)
        -0.2822580337524414)

  ; Single precision, offset 3, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-single-ref bytevector-ieee-single-set!
                  3 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-single-ref bytevector-ieee-single-set! 3 'little)
        -0.2822580337524414)

  ; Double precision, offset 0, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  0 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'big)
        -0.2822580337524414)

  ; Double precision, offset 0, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  0 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 0 'little)
        -0.2822580337524414)

  ; Double precision, offset 1, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  1 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'big)
        -0.2822580337524414)

  ; Double precision, offset 1, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  1 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 1 'little)
        -0.2822580337524414)

  ; Double precision, offset 2, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  2 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'big)
        -0.2822580337524414)

  ; Double precision, offset 2, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  2 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 2 'little)
        -0.2822580337524414)

  ; Double precision, offset 3, big-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  3 'big)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'big)
        -0.2822580337524414)

  ; Double precision, offset 3, little-endian

  (tst1 (roundtrip
         +inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        +inf.0)

  (tst1 (roundtrip
         -inf.0
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        -inf.0)

  (tst1 (let ((x (roundtrip
                  +nan.0
                  bytevector-ieee-double-ref bytevector-ieee-double-set!
                  3 'little)))
          (= x x))
        #f)

  (tst1 (roundtrip
         1e10
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        1e10)

  (tst1 (roundtrip
         -0.2822580337524414
         bytevector-ieee-double-ref bytevector-ieee-double-set! 3 'little)
        -0.2822580337524414)

  (allof "IEEE representations" (reverse results))

  (set! results '())

  ; Denormalized numbers.

  (do ((x (expt .5 100) (* .5 x)))
      ((= x 0.0))
    (let ((y (->single x)))
      (if (> y 0.0)
          (tst1 y x))))

  (allof "Denormalized single" (reverse results))

  (set! results '())

  (do ((x (expt .5 1000) (* .5 x)))
      ((= x 0.0))
    (let ((y (->double x)))
      (tst1 y x)))

  (allof "Denormalized double" (reverse results)))



(define (string-bytevector-tests)

  (define (test-roundtrip bvec tostring tobvec)
    (let* ((s1 (tostring bvec))
           (b2 (tobvec s1))
           (s2 (tostring b2)))
      (test "round trip of string conversion" (string=? s1 s2) #t)))

  (define random
    (letrec ((random14
              (lambda (n)
                (set! x (remainder (+ (* a x) c) (+ m 1)))
                (remainder (quotient x 8) n)))
             (a 701)
             (x 1)
             (c 743483)
             (m 524287)
             (loop
              (lambda (q r n)
                (if (zero? q)
                    (remainder r n)
                    (loop (quotient q 16384)
                          (+ (* 16384 r) (random14 16384))
                          n)))))
      (lambda (n)
        (if (< n 16384)
            (random14 n)
            (loop (quotient n 16384) (random14 16384) n)))))
 
  ; Returns a random bytevector of length up to n.

  (define (random-bytevector n)
    (let* ((n (random n))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 256)))))

  ; Returns a random bytevector of even length up to n.

  (define (random-bytevector2 n)
    (let* ((n (random n))
           (n (if (odd? n) (+ n 1) n))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 256)))))

  ; Returns a random bytevector of multiple-of-4 length up to n.

  (define (random-bytevector4 n)
    (let* ((n (random n))
           (n (* 4 (round (/ n 4))))
           (bv (make-bytevector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) bv)
        (bytevector-u8-set! bv i (random 256)))))

  (test "utf-8, BMP"
        (bytevector=? (string->utf8 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                      '#vu8(#x6b
                            #x7f
                            #b11000010 #b10000000
                            #b11011111 #b10111111
                            #b11100000 #b10100000 #b10000000
                            #b11101111 #b10111111 #b10111111))
        #t)

  (test "utf-8, supplemental"
        (bytevector=? (string->utf8 "\x010000;\x10ffff;")
                      '#vu8(#b11110000 #b10010000 #b10000000 #b10000000
                            #b11110100 #b10001111 #b10111111 #b10111111))
        #t)

  (test "utf-8, errors 1"
        (string=? (utf8->string '#vu8(#x61                             ; a
                                      #xc0 #x62                        ; ?b
                                      #xc1 #x63                        ; ?c
                                      #xc2 #x64                        ; ?d
                                      #x80 #x65                        ; ?e
                                      #xc0 #xc0 #x66                   ; ??f
                                      #xe0 #x67                        ; ?g
                                     ))
                  "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e\xfffd;\xfffd;f\xfffd;g")
        #t)

  (test "utf-8, errors 2"
        (string=? (utf8->string '#vu8(#xe0 #x80 #x80 #x68              ; ???h
                                      #xe0 #xc0 #x80 #x69              ; ???i
                                      #xf0 #x6a                        ; ?j
                                     ))
                  "\xfffd;\xfffd;\xfffd;h\xfffd;\xfffd;\xfffd;i\xfffd;j")
        #t)

  (test "utf-8, errors 3"
        (string=? (utf8->string '#vu8(#x61                             ; a
                                      #xf0 #x80 #x80 #x80 #x62         ; ????b
                                      #xf0 #x90 #x80 #x80 #x63         ; .c
                                     ))
                  "a\xfffd;\xfffd;\xfffd;\xfffd;b\x10000;c")
        #t)

  (test "utf-8, errors 4"
        (string=? (utf8->string '#vu8(#x61                             ; a
                                      #xf0 #xbf #xbf #xbf #x64         ; .d
                                      #xf0 #xbf #xbf #x65              ; ?e
                                      #xf0 #xbf #x66                   ; ?f
                                     ))
                  "a\x3ffff;d\xfffd;e\xfffd;f")
        #t)

  (test "utf-8, errors 5"
        (string=? (utf8->string '#vu8(#x61                             ; a
                                      #xf4 #x8f #xbf #xbf #x62         ; .b
                                      #xf4 #x90 #x80 #x80 #x63         ; ????c
                                     ))

                  "a\x10ffff;b\xfffd;\xfffd;\xfffd;\xfffd;c")
        #t)

  (test "utf-8, errors 6"
        (string=? (utf8->string '#vu8(#x61                             ; a
                                      #xf5 #x80 #x80 #x80 #x64         ; ????d
                                     ))

                  "a\xfffd;\xfffd;\xfffd;\xfffd;d")
        #t)

  ; ignores BOM signature

  (test "utf-8, BOM"
        (string=? (utf8->string '#vu8(#xef #xbb #xbf #x61 #x62 #x63 #x64))
                  "abcd")
        #t)

  (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

  (do ((i 0 (+ i 1)))
      ((= i *random-stress-tests*))
    (test-roundtrip (random-bytevector *random-stress-test-max-size*)
                    utf8->string string->utf8))

  (test "utf-16, BMP"
        (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
                      '#vu8(#x00 #x6b
                            #x00 #x7f
                            #x00 #x80
                            #x07 #xff
                            #x08 #x00
                            #xff #xff))
        #t)

  (test "utf-16le, BMP"
        (bytevector=? (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                                     'little)
                      '#vu8(#x6b #x00
                            #x7f #x00
                            #x80 #x00
                            #xff #x07
                            #x00 #x08
                            #xff #xff))
        #t)

  (test "utf-16, supplemental"
        (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;")
                      '#vu8(#xd8 #x00 #xdc #x00
                            #xdb #xb7 #xdc #xba
                            #xdb #xff #xdf #xff))
        #t)

  (test "utf-16le, supplemental"
        (bytevector=? (string->utf16 "\x010000;\xfdcba;\x10ffff;" 'little)
                      '#vu8(#x00 #xd8 #x00 #xdc
                            #xb7 #xdb #xba #xdc
                            #xff #xdb #xff #xdf))
        #t)

  (test "utf-16be"
        (bytevector=? (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd")
                      (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd" 'big))
        #t)

  (test "utf-16, errors 1"
        (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                  (utf16->string
                   '#vu8(#x00 #x6b
                         #x00 #x7f
                         #x00 #x80
                         #x07 #xff
                         #x08 #x00
                         #xff #xff)))
        #t)

  (test "utf-16, errors 2"
        (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                  (utf16->string
                   '#vu8(#x00 #x6b
                         #x00 #x7f
                         #x00 #x80
                         #x07 #xff
                         #x08 #x00
                         #xff #xff)
                   'big))
        #t)

  (test "utf-16, errors 3"
        (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                  (utf16->string
                   '#vu8(#xfe #xff     ; big-endian BOM
                         #x00 #x6b
                         #x00 #x7f
                         #x00 #x80
                         #x07 #xff
                         #x08 #x00
                         #xff #xff)))
        #t)

  (test "utf-16, errors 4"
        (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                  (utf16->string
                   '#vu8(#x6b #x00
                         #x7f #x00
                         #x80 #x00
                         #xff #x07
                         #x00 #x08
                         #xff #xff)
                   'little))
        #t)

  (test "utf-16, errors 5"
        (string=? "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
                  (utf16->string
                   '#vu8(#xff #xfe     ; little-endian BOM
                         #x6b #x00
                         #x7f #x00
                         #x80 #x00
                         #xff #x07
                         #x00 #x08
                         #xff #xff)))
        #t)

  (let ((tostring        utf16->string)
        (tostring-big    (lambda (bv) (utf16->string bv 'big)))
        (tostring-little (lambda (bv) (utf16->string bv 'little)))
        (tobvec          string->utf16)
        (tobvec-big      (lambda (s) (string->utf16 s 'big)))
        (tobvec-little   (lambda (s) (string->utf16 s 'little))))

    (do ((i 0 (+ i 1)))
        ((= i *random-stress-tests*))
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring tobvec)
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring-big tobvec-big)
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring-little tobvec-little)))

  (test "utf-32"
        (bytevector=? (string->utf32 "abc")
                      '#vu8(#x00 #x00 #x00 #x61
                            #x00 #x00 #x00 #x62
                            #x00 #x00 #x00 #x63))
        #t)

  (test "utf-32be"
        (bytevector=? (string->utf32 "abc" 'big)
                      '#vu8(#x00 #x00 #x00 #x61
                            #x00 #x00 #x00 #x62
                            #x00 #x00 #x00 #x63))
        #t)

  (test "utf-32le"
        (bytevector=? (string->utf32 "abc" 'little)
                      '#vu8(#x61 #x00 #x00 #x00
                            #x62 #x00 #x00 #x00
                            #x63 #x00 #x00 #x00))
        #t)

  (test "utf-32, errors 1"
        (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#x00 #x00 #x00 #x61
                         #x00 #x00 #xd9 #x00
                         #x00 #x00 #x00 #x62
                         #x00 #x00 #xdd #xab
                         #x00 #x00 #x00 #x63
                         #x00 #x11 #x00 #x00
                         #x00 #x00 #x00 #x64
                         #x01 #x00 #x00 #x65
                         #x00 #x00 #x00 #x65)))
        #t)

  (test "utf-32, errors 2"
        (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#x00 #x00 #x00 #x61
                         #x00 #x00 #xd9 #x00
                         #x00 #x00 #x00 #x62
                         #x00 #x00 #xdd #xab
                         #x00 #x00 #x00 #x63
                         #x00 #x11 #x00 #x00
                         #x00 #x00 #x00 #x64
                         #x01 #x00 #x00 #x65
                         #x00 #x00 #x00 #x65)
                   'big))
        #t)

  (test "utf-32, errors 3"
        (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                         #x00 #x00 #x00 #x61
                         #x00 #x00 #xd9 #x00
                         #x00 #x00 #x00 #x62
                         #x00 #x00 #xdd #xab
                         #x00 #x00 #x00 #x63
                         #x00 #x11 #x00 #x00
                         #x00 #x00 #x00 #x64
                         #x01 #x00 #x00 #x65
                         #x00 #x00 #x00 #x65)))
        #t)

  (test "utf-32, errors 4"
        (string=? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#x00 #x00 #xfe #xff   ; big-endian BOM
                         #x00 #x00 #x00 #x61
                         #x00 #x00 #xd9 #x00
                         #x00 #x00 #x00 #x62
                         #x00 #x00 #xdd #xab
                         #x00 #x00 #x00 #x63
                         #x00 #x11 #x00 #x00
                         #x00 #x00 #x00 #x64
                         #x01 #x00 #x00 #x65
                         #x00 #x00 #x00 #x65)
                   'big
                   #t))
        #t)

  (test "utf-32, errors 5"
        (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#x61 #x00 #x00 #x00
                         #x00 #xd9 #x00 #x00
                         #x62 #x00 #x00 #x00
                         #xab #xdd #x00 #x00
                         #x63 #x00 #x00 #x00
                         #x00 #x00 #x11 #x00
                         #x64 #x00 #x00 #x00
                         #x65 #x00 #x00 #x01
                         #x65 #x00 #x00 #x00)
                   'little))
        #t)

  (test "utf-32, errors 6"
        (string=? "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                         #x61 #x00 #x00 #x00
                         #x00 #xd9 #x00 #x00
                         #x62 #x00 #x00 #x00
                         #xab #xdd #x00 #x00
                         #x63 #x00 #x00 #x00
                         #x00 #x00 #x11 #x00
                         #x64 #x00 #x00 #x00
                         #x65 #x00 #x00 #x01
                         #x65 #x00 #x00 #x00)))
        #t)

  (test "utf-32, errors 7"
        (string=? "\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
                  (utf32->string
                   '#vu8(#xff #xfe #x00 #x00   ; little-endian BOM
                         #x61 #x00 #x00 #x00
                         #x00 #xd9 #x00 #x00
                         #x62 #x00 #x00 #x00
                         #xab #xdd #x00 #x00
                         #x63 #x00 #x00 #x00
                         #x00 #x00 #x11 #x00
                         #x64 #x00 #x00 #x00
                         #x65 #x00 #x00 #x01
                         #x65 #x00 #x00 #x00)
                   'little
                   #t))
        #t)

  (let ((tostring        utf32->string)
        (tostring-big    (lambda (bv) (utf32->string bv 'big)))
        (tostring-little (lambda (bv) (utf32->string bv 'little)))
        (tobvec          string->utf32)
        (tobvec-big      (lambda (s) (string->utf32 s 'big)))
        (tobvec-little   (lambda (s) (string->utf32 s 'little))))

    (do ((i 0 (+ i 1)))
        ((= i *random-stress-tests*))
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring tobvec)
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring-big tobvec-big)
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring-little tobvec-little)))

)

(define *random-stress-tests* 100)            ; number of tests
(define *random-stress-test-max-size* 50)     ; twice average size of string

; Tests string <-> bytevector conversion on strings
; that contain every Unicode scalar value.

(define (exhaustive-string-bytevector-tests)

  ; Tests throughout an inclusive range.

  (define (test-char-range lo hi tostring tobytevector)
    (let* ((n (+ 1 (- hi lo)))
           (s (make-string n))
           (replacement-character (integer->char #xfffd)))
      (do ((i lo (+ i 1)))
          ((> i hi))
        (let ((c (if (or (<= 0 i #xd7ff)
                         (<= #xe000 i #x10ffff))
                     (integer->char i)
                     replacement-character)))
          (string-set! s (- i lo) c)))
      (test "test of long string conversion"
            (string=? (tostring (tobytevector s)) s) #t)))

  (define (test-exhaustively name tostring tobytevector)
   ;(display "Testing ")
   ;(display name)
   ;(display " conversions...")
   ;(newline)
    (test-char-range 0 #xffff tostring tobytevector)
    (test-char-range #x10000 #x1ffff tostring tobytevector)
    (test-char-range #x20000 #x2ffff tostring tobytevector)
    (test-char-range #x30000 #x3ffff tostring tobytevector)
    (test-char-range #x40000 #x4ffff tostring tobytevector)
    (test-char-range #x50000 #x5ffff tostring tobytevector)
    (test-char-range #x60000 #x6ffff tostring tobytevector)
    (test-char-range #x70000 #x7ffff tostring tobytevector)
    (test-char-range #x80000 #x8ffff tostring tobytevector)
    (test-char-range #x90000 #x9ffff tostring tobytevector)
    (test-char-range #xa0000 #xaffff tostring tobytevector)
    (test-char-range #xb0000 #xbffff tostring tobytevector)
    (test-char-range #xc0000 #xcffff tostring tobytevector)
    (test-char-range #xd0000 #xdffff tostring tobytevector)
    (test-char-range #xe0000 #xeffff tostring tobytevector)
    (test-char-range #xf0000 #xfffff tostring tobytevector)
    (test-char-range #x100000 #x10ffff tostring tobytevector))

  ; Feel free to replace this with your favorite timing macro.

  (define (timeit x) x)

  (timeit (test-exhaustively "UTF-8" utf8->string string->utf8))

  (timeit (test-exhaustively "UTF-16" utf16->string string->utf16))

  (timeit (test-exhaustively "UTF-16BE"
                             (lambda (bv) (utf16->string bv 'big))
                             (lambda (s) (string->utf16 s 'big))))

  (timeit (test-exhaustively "UTF-16LE"
                             (lambda (bv) (utf16->string bv 'little))
                             (lambda (s) (string->utf16 s 'little))))

  (timeit (test-exhaustively "UTF-32" utf32->string string->utf32))

  (timeit (test-exhaustively "UTF-32BE"
                             (lambda (bv) (utf32->string bv 'big))
                             (lambda (s) (string->utf32 s 'big))))

  (timeit (test-exhaustively "UTF-32LE"
                             (lambda (bv) (utf32->string bv 'little))
                             (lambda (s) (string->utf32 s 'little)))))


