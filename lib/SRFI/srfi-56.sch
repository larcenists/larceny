;;; Copyright (c) 2004-2005 by Alex Shinn. Details at end of file.

;; constants
(define *byte-size* 8)
(define *byte-magnitude* (expt 2 *byte-size*))
(define *byte-mask* (- *byte-magnitude* 1))
(define *byte-right-shift* (* -1 *byte-size*))

;; XXXX default endianess (platform specific, these defaults are for x86)
(define *default-endian* 'little-endian)
(define *default-float-endian* 'little-endian)

(define (default-endian) *default-endian*)
(define (default-float-endian) *default-float-endian*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XXXX implementation-specific primitives

;; comment these out in Scheme48 and run ,open ascii
(define ascii->char integer->char)
(define char->ascii char->integer)

;; comment these out in MzScheme >= 299
(define (read-byte . opt)
  (let ((c (apply read-char opt)))
    (if (eof-object? c) c (char->ascii c))))
(define (write-byte int . opt)
  (apply write-char (ascii->char int) opt))
(define (peek-byte . opt)
  (let ((c (apply peek-char opt)))
    (if (eof-object? c) c (char->ascii c))))
(define byte-ready? char-ready?)

;; don't differentiate between binary and character ports
(define (port? x) (or (input-port? x) (output-port? x)))
(define binary-port? port?)
(define character-port? port?)
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define call-with-binary-input-file call-with-input-file)
(define call-with-binary-output-file call-with-output-file)
(define with-input-from-binary-file with-input-from-file)
(define with-output-to-binary-file with-output-to-file)

;; SISC, comment out the above and use
; (import binary-io)
; (define (binary-port? o)
;   (or (binary-input-port? o) (binary-output-port? o)))
; (define (character-port? o)
;   (and (port? o) (not (binary-port? o))))
; (define with-input-from-binary-file with-binary-input-from-file)
; (define with-output-to-binary-file with-binary-output-to-file)

;; use these in Kawa
; (define (kawaify f)
;   (lambda args (fluid-let ((port-char-encoding #f)) (apply f args))))
; (define open-binary-input-file (kawaify open-input-file))
; (define open-binary-output-file (kawaify open-output-file))
; (define call-with-binary-input-file (kawaify call-with-input-file))
; (define call-with-binary-output-file (kawaify call-with-output-file))
; (define with-input-from-binary-file (kawaify with-input-from-file))
; (define with-output-to-binary-file (kawaify with-output-to-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitwise operators

;; XXXX we use the following from SRFI-60, defined here in terms of
;; Kawa/Guile/Gauche style names
; (define arithmetic-shift ash) ; comment this out in Kawa
; (define bitwise-and logand)
; (define bitwise-ior logior)
; (define bitwise-not lognot)

;; XXXX portable but **VERY** slow!  SLIB may be faster, native is best
(define (arithmetic-shift a i)
  (if (negative? i)
    (quotient a (expt 2 (- i)))
    (* a (expt 2 i))))
(define (bitwise-and a b)
  (cond
    ((zero? a) 0)
    ((zero? b) 0)
    (else
     (+ (if (and (odd? a) (odd? b)) 1 0)
        (* 2 (bitwise-and (quotient a 2) (quotient b 2)))))))
(define (bitwise-ior a b)
  (cond
    ((zero? a) b)
    ((zero? b) a)
    (else
     (+ (if (or (odd? a) (odd? b)) 1 0)
        (* 2 (bitwise-ior (quotient a 2) (quotient b 2)))))))
(define (bitwise-xor a b)
  (+ (if (if (odd? a) (not (odd? b)) (odd? b)) 1 0)
     (* 2 (bitwise-xor (quotient a 2) (quotient b 2)) 1)))
(define (bitwise-not a)
  (- -1 a))

;; from the SRFI-33 reference implementation
(define (extract-bit-field size position n)
  (bitwise-and (bitwise-not (arithmetic-shift -1 size))
               (arithmetic-shift n (- position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax (with optional low-level macro definitions)

;; like let-optionals* but uses default if #f is specified
(define-syntax let-params*
  (syntax-rules ()
    ((_ ls () . body) (let () . body))
    ((_ ls ((var default) rest ...) . body)
     (let* ((tmp ls)
            (var (or (and (pair? tmp) (car tmp)) default)))
       (let-params* (if (pair? tmp) (cdr tmp) '()) (rest ...) . body)))
    ((_ ls ((var) rest ...) . body)
     (let-params* ls ((var #f) rest ...) . body))))

; (define-macro (let-params* ls params . body)
;   (if (null? params)
;     `(let () ,@body)
;     (let ((tmp (gensym)))
;       `(let* ((,tmp ,ls)
;               (,(caar params)
;                (or (and (pair? ,tmp) (car ,tmp)) ,(cadar params))))
;          (let-params* (if (pair? ,tmp) (cdr ,tmp) '()) ,(cdr params)
;            ,@body)))))

;; simplified version Oleg's good assert (no report:)
(define-syntax assert
  (syntax-rules ()
    ((_) #t)
    ((_ test rest ...)
     (let ((message (quote test)))
       (if (not test)
         ;;(error "" "test failed:" message) ; use this for bigloo
         (error "test failed:" message)
         (assert rest ...))))))

; (define-macro (assert . o) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library code.  Below here only optimization changes should be needed.

;; basic reading

(define (combine . bytes)
  (combine-ls bytes))

(define (combine-ls bytes)
  (let loop ((b bytes) (acc 0))
    (if (null? b) acc
        (loop (cdr b) (+ (arithmetic-shift acc 8) (car b))))))

(define (read-binary-uint size . opt)
  (assert (integer? size) (positive? size))
  (let-params* opt ((port (current-input-port)) (endian *default-endian*))
    (let loop ((i size) (ls '()))
      (if (zero? i)
        (combine-ls (if (eq? endian 'big-endian) (reverse ls) ls))
        (let ((b (read-byte port)))
          (if (eof-object? b) b (loop (- i 1) (cons b ls))))))))

(define (read-binary-sint size . opt)
  (assert (integer? size) (positive? size))
  (let-params* opt ((port (current-input-port)) (endian *default-endian*))
    (let loop ((i size) (ls '()))
      (if (zero? i)
        (let ((bytes (if (eq? endian 'big-endian) (reverse ls) ls)))
          (if (> (car bytes) 127)
            (* -1 (+ 1 (combine-ls (map (lambda (b) (- 255 b)) bytes))))
            (combine-ls bytes)))
        (let ((b (read-byte port)))
          (if (eof-object? b) b (loop (- i 1) (cons b ls))))))))

(define (read-binary-uint8 . x) ; takes optional endian, unlike read-byte
  (read-byte (or (and (pair? x) (car x)) (current-input-port))))
(define (read-binary-uint16 . x) (apply read-binary-uint 2 x))
(define (read-binary-uint32 . x) (apply read-binary-uint 4 x))
(define (read-binary-uint64 . x) (apply read-binary-uint 8 x))

(define (read-binary-sint8 . x)  (apply read-binary-sint 1 x))
(define (read-binary-sint16 . x) (apply read-binary-sint 2 x))
(define (read-binary-sint32 . x) (apply read-binary-sint 4 x))
(define (read-binary-sint64 . x) (apply read-binary-sint 8 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic writing

(define (write-binary-uint size int . opt)
  (assert (integer? size) (positive? size)
          (integer? int) (not (negative? int)))
  (let-params* opt ((port (current-output-port)) (endian *default-endian*))
    (let loop ((i size) (n int) (ls '()))
      (if (zero? i)
        (for-each (lambda (b) (write-byte b port))
                  (if (eq? endian 'big-endian)
                    ls (reverse ls)))
        (loop (- i 1) (arithmetic-shift n *byte-right-shift*)
              (cons (bitwise-and n *byte-mask*) ls))))))

(define (write-binary-sint size int . opt)
  (assert (integer? size) (positive? size) (integer? int))
  (let-params* opt ((port (current-output-port)) (endian *default-endian*))
    (let loop ((i size) (n (if (negative? int) (- -1 int) int)) (ls '()))
      (if (zero? i)
        (for-each
         (lambda (b) (write-byte b port))
         ((if (negative? int)
            (lambda (ls) (map (lambda (x) (- 255 x)) ls))
            (lambda (x) x))
          (if (eq? endian 'big-endian)
            ls (reverse ls))))
        (loop (- i 1) (arithmetic-shift n *byte-right-shift*)
              (cons (bitwise-and n *byte-mask*) ls))))))

(define write-binary-uint8         write-byte)
(define (write-binary-uint16 . x)  (apply write-binary-uint 2 x))
(define (write-binary-uint32 . x)  (apply write-binary-uint 4 x))
(define (write-binary-uint64 . x)  (apply write-binary-uint 8 x))

(define (write-binary-sint8 . x)   (apply write-binary-sint 1 x))
(define (write-binary-sint16 . x)  (apply write-binary-sint 2 x))
(define (write-binary-sint32 . x)  (apply write-binary-sint 4 x))
(define (write-binary-sint64 . x)  (apply write-binary-sint 8 x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nextwork encodings

;; XXXX these may be defined as aliases for the binary equivalents if
;; your architecture is big-endian.

(define (/opt o) (and (pair? o) (car o)))

(define (read-network-uint16 . o)
  (read-binary-uint 2 (/opt o) 'big-endian))
(define (read-network-uint32 . o)
  (read-binary-uint 4 (/opt o) 'big-endian))
(define (read-network-uint64 . o)
  (read-binary-uint 8 (/opt o) 'big-endian))

(define (read-network-sint16 . o)
  (read-binary-sint 2 (/opt o) 'big-endian))
(define (read-network-sint32 . o)
  (read-binary-sint 4 (/opt o) 'big-endian))
(define (read-network-sint64 . o)
  (read-binary-sint 8 (/opt o) 'big-endian))

(define (write-network-uint16 x . o)
  (write-binary-uint 2 x (/opt o) 'big-endian))
(define (write-network-uint32 x . o)
  (write-binary-uint 4 x (/opt o) 'big-endian))
(define (write-network-uint64 x . o)
  (write-binary-uint 8 x (/opt o) 'big-endian))

(define (write-network-sint16 x . o)
  (write-binary-sint 2 x (/opt o) 'big-endian))
(define (write-network-sint32 x . o)
  (write-binary-sint 4 x (/opt o) 'big-endian))
(define (write-network-sint64 x . o)
  (write-binary-sint 8 x (/opt o) 'big-endian))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bignum encodings -- Basic Encoding Rules (BER) from X.209

;; A BER compressed integer is an unsigned integer in base 128, most
;; significant digit first, where the high bit is set on all but the
;; final (least significant) byte.  Thus any size integer can be
;; encoded, but the encoding is efficient and small integers don't take
;; up any more space than they would in normal char/short/int encodings.

(define (read-ber-integer . opt)
  (let-params* opt ((port (current-input-port)))
    (let loop ((acc 0))
      (let ((byte (read-byte port)))
        (cond
          ((eof-object? byte) byte)   ; fail on eof
          ((< byte 128) (+ acc byte)) ; final byte is < 128
          (else
           (loop (arithmetic-shift (+ acc (bitwise-and byte 127)) 7))))))))

(define (write-ber-integer number . opt)
  (assert (integer? number) (not (negative? number)))
  (let-params* opt ((port (current-output-port)))
    (let loop ((n (arithmetic-shift number -7))
               (ls (list (bitwise-and number 127))))
      (if (zero? n)
        (for-each (lambda (b) (write-byte b port)) ls)
        (loop (arithmetic-shift n -7)
              (cons (+ 128 (bitwise-and n 127)) ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading floating point numbers

;; Inspired by Oleg's implementation from
;;   http://okmij.org/ftp/Scheme/reading-IEEE-floats.txt
;; but removes mutations and magic numbers and allows for manually
;; specifying the endianess.
;;
;; See also
;;   http://www.cs.auckland.ac.nz/~jham1/07.211/floats.html
;; and
;;   http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
;; as references to IEEE 754.

(define (read-ieee-float32 . opt)
  (let-params* opt ((port (current-input-port))
                    (endian *default-float-endian*))
    (define (mantissa expn b2 b3 b4)
      (case expn   ; recognize special literal exponents
        ((255)
         ;;(if (zero? (combine b2 b3 b4)) +/0. 0/0.) ; XXXX for SRFI-70
         #f)
        ((0)       ; denormalized
         (exact->inexact (* (expt 2.0 (- 1 (+ 127 23))) (combine b2 b3 b4))))
        (else
         (exact->inexact
          (* (expt 2.0 (- expn (+ 127 23)))
             (combine (+ b2 128) b3 b4)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4)
      (if (> b2 127)  ; 1st bit of b2 is low bit of expn
        (mantissa (+ (* 2 b1) 1) (- b2 128) b3 b4)
        (mantissa (* 2 b1) b2 b3 b4)))
    (define (sign b1 b2 b3 b4)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4) => -) (else #f))
        (exponent b1 b2 b3 b4)))
    (let* ((b1 (read-byte port))  (b2 (read-byte port))
           (b3 (read-byte port))  (b4 (read-byte port)))
      (if (eof-object? b4)
        b4
        (if (eq? endian 'big-endian)
          (sign b1 b2 b3 b4)
          (sign b4 b3 b2 b1))))))

(define (read-ieee-float64 . opt)
  (let-params* opt ((port (current-input-port))
                    (endian *default-float-endian*))
    (define (mantissa expn b2 b3 b4 b5 b6 b7 b8)
      (case expn   ; recognize special literal exponents
        ((255) #f) ; won't handle NaN and +/- Inf
        ((0)       ; denormalized
         (exact->inexact (* (expt 2.0 (- 1 (+ 1023 52)))
                            (combine b2 b3 b4 b5 b6 b7 b8))))
        (else
         (exact->inexact
          (* (expt 2.0 (- expn (+ 1023 52)))
             (combine (+ b2 16) b3 b4 b5 b6 b7 b8)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4 b5 b6 b7 b8)
      (mantissa (bitwise-ior (arithmetic-shift b1 4)          ; 7 bits
                             (extract-bit-field 4 4 b2))      ; + 4 bits
                (extract-bit-field 4 0 b2) b3 b4 b5 b6 b7 b8))
    (define (sign b1 b2 b3 b4 b5 b6 b7 b8)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4 b5 b6 b7 b8) => -)
              (else #f))
        (exponent b1 b2 b3 b4 b5 b6 b7 b8)))
    (let* ((b1 (read-byte port))  (b2 (read-byte port))
           (b3 (read-byte port))  (b4 (read-byte port))
           (b5 (read-byte port))  (b6 (read-byte port))
           (b7 (read-byte port))  (b8 (read-byte port)))
      (if (eof-object? b8)
        b8
        (if (eq? endian 'big-endian)
          (sign b1 b2 b3 b4 b5 b6 b7 b8)
          (sign b8 b7 b6 b5 b4 b3 b2 b1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing floating point numbers

;; Underflow rounds down to zero as in IEEE-754, and overflow gets
;; written as +/- Infinity.

;; Break a real number down to a normalized mantissa and exponent.
;; Default base=2, mant-size=23 (52), exp-size=8 (11) for IEEE singles
;; (doubles).
;;
;; Note: This should never be used in practice, since it can be
;; implemented much faster in C.  See decode-float in ChezScheme or
;; Gauche.
(define (call-with-mantissa&exponent num . opt)
  (define (last ls)
    (if (null? (cdr ls)) (car ls) (last (cdr ls))))
  (define (with-last&rest ls proc)
    (let lp ((ls ls) (res '()))
      (if (null? (cdr ls))
        (proc (car ls) (reverse res))
        (lp (cdr ls) (cons (car ls) res)))))
  (cond
    ((negative? num) (apply call-with-mantissa&exponent (- num) opt))
    ((zero? num) ((last opt) 0 0))
    (else
     (with-last&rest opt
       (lambda (proc params)
         (let-params* params ((base 2) (mant-size 23) (exp-size 8))
           (let* ((bot (expt base mant-size))
                  (top (* base bot)))
             (let loop ((n (exact->inexact num)) (e 0))
               (cond
                 ((>= n top)
                  (loop (/ n base) (+ e 1)))
                 ((< n bot)
                  (loop (* n base) (- e 1)))
                 (else
                  (proc (inexact->exact (round n)) e)))))))))))

(define (write-ieee-float32 num . opt)
  (assert (real? num))
  (let-params* opt ((port (current-output-port))
                    (endian *default-float-endian*))
    (define (bytes)
      (call-with-mantissa&exponent num
        (lambda (f e)
          (let ((e0 (+ e 127 23)))
            (cond
              ((negative? e0)
               (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (extract-bit-field 7 16 f1))        ; mant:16-23
                      (b3 (extract-bit-field 8 8 f1))         ; mant:8-15
                      (b4 (extract-bit-field 8 0 f1)))        ; mant:0-7
                 (list (if (negative? num) 128 0) b2 b3 b4)))
              ((> e0 255) ; XXXX here we just write infinity
               (list (if (negative? num) 255 127) 128 0 0))
              (else
               (let* ((b0 (arithmetic-shift e0 -1))
                      (b1 (if (negative? num) (+ b0 128) b0)) ; sign + exp:1-7
                      (b2 (bitwise-ior
                           (if (odd? e0) 128 0)               ; exp:0
                           (extract-bit-field 7 16 f)))       ;   + mant:16-23
                      (b3 (extract-bit-field 8 8 f))          ; mant:8-15
                      (b4 (extract-bit-field 8 0 f)))         ; mant:0-7
                 (list b1 b2 b3 b4))))))))
    (for-each
     (lambda (b) (write-byte b port))
     (cond ((zero? num) '(0 0 0 0))
           ((eq? endian 'big-endian) (bytes))
           (else (reverse (bytes)))))))

(define (write-ieee-float64 num . opt)
  (assert (real? num))
  (let-params* opt ((port (current-output-port))
                    (endian *default-float-endian*))
    (define (bytes)
      (call-with-mantissa&exponent num 2 52 11
        (lambda (f e)
          (let ((e0 (+ e 1023 52)))
            (cond
              ((negative? e0)
               (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (extract-bit-field 4 48 f1))
                      (b3 (extract-bit-field 8 40 f1))
                      (b4 (extract-bit-field 8 32 f1))
                      (b5 (extract-bit-field 8 24 f1))
                      (b6 (extract-bit-field 8 16 f1))
                      (b7 (extract-bit-field 8 8 f1))
                      (b8 (extract-bit-field 8 0 f1)))
                 (list (if (negative? num) 128 0) b2 b3 b4 b5 b6 b7 b8)))
              ((> e0 4095) ; infinity
               (list (if (negative? num) 255 127) 224 0 0 0 0 0 0))
              (else
               (let* ((b0 (extract-bit-field 7 4 e0))
                      (b1 (if (negative? num) (+ b0 128) b0))
                      (b2 (bitwise-ior (arithmetic-shift
                                        (extract-bit-field 4 0 e0)
                                        4)
                                       (extract-bit-field 4 48 f)))
                      (b3 (extract-bit-field 8 40 f))
                      (b4 (extract-bit-field 8 32 f))
                      (b5 (extract-bit-field 8 24 f))
                      (b6 (extract-bit-field 8 16 f))
                      (b7 (extract-bit-field 8 8 f))
                      (b8 (extract-bit-field 8 0 f)))
                 (list b1 b2 b3 b4 b5 b6 b7 b8))))))))
    (for-each
     (lambda (b) (write-byte b port))
     (cond ((zero? num) '(0 0 0 0 0 0 0 0))
           ((eq? endian 'big-endian) (bytes))
           (else (reverse (bytes)))))))

;;; Copyright (c) 2004-2005 by Alex Shinn. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
