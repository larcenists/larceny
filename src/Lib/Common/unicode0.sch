;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Temporary code extracted from larceny.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

($$trace "unicode0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (r6rs base)
;
; Implements only as much as is used by enumerations.sch.
;
; Except for the library syntax, the reference implementation
; uses only the R5RS-compatible procedures from this library.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given exact integers n and k, with k >= 0, return (* n (expt 2 k)).

(define (arithmetic-shift n k)
  (if (and (exact? n)
           (integer? n)
           (exact? k)
           (integer? k))
      (cond ((> k 0)
             (* n (expt 2 k)))
            ((= k 0)
             n)
            ((>= n 0)
             (quotient n (expt 2 (- k))))
            (else
             (let* ((q (expt 2 (- k)))
                    (p (quotient (- n) q)))
               (if (= n (* p k))
                   (- p)
                   (- -1 p)))))
      (error "illegal argument to arithmetic-shift" n k)))

; Bitwise operations on exact integers.

(define (bitwise-and i j)
  (if (and (exact? i)
           (integer? i)
           (exact? j)
           (integer? j))
      (cond ((or (= i 0) (= j 0))
             0)
            ((= i -1)
             j)
            ((= j -1)
             i)
            (else
             (let* ((i0 (if (odd? i) 1 0))
                    (j0 (if (odd? j) 1 0))
                    (i1 (- i i0))
                    (j1 (- j j0))
                    (i/2 (quotient i1 2))
                    (j/2 (quotient j1 2))
                    (hi (* 2 (bitwise-and i/2 j/2)))
                    (lo (* i0 j0)))
               (+ hi lo))))
      (error "illegal argument to bitwise-and" i j)))

(define (bitwise-ior i j)
  (if (and (exact? i)
           (integer? i)
           (exact? j)
           (integer? j))
      (cond ((or (= i -1) (= j -1))
             -1)
            ((= i 0)
             j)
            ((= j 0)
             i)
            (else
             (let* ((i0 (if (odd? i) 1 0))
                    (j0 (if (odd? j) 1 0))
                    (i1 (- i i0))
                    (j1 (- j j0))
                    (i/2 (quotient i1 2))
                    (j/2 (quotient j1 2))
                    (hi (* 2 (bitwise-ior i/2 j/2)))
                    (lo (if (= 0 (+ i0 j0)) 0 1)))
               (+ hi lo))))
      (error "illegal argument to bitwise-ior" i j)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (r6rs bytevector)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-bytevector is already provided by Larceny
; bytevector-length is already provided by Larceny

(define bytevector-u8-ref bytevector-ref)
(define bytevector-u8-set! bytevector-set!)
(define u8-list->bytevector list->bytevector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of temporary code extracted from larceny.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Copyright 2006 William D Clinger.
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
; (proto-unicode0)
;
; This file contains a few auxiliary procedures that are used
; by the reference implementation of the (r6rs unicode) library.
; None of the definitions in this file are exported as part of
; (r6rs unicode).
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

;(library (proto-unicode0)
;  (export
;
;    binary-search-of-vector
;    binary-search
;    binary-search-16bit)
;
;  (import (r6rs base)
;          (r6rs bytevector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures (not part of R6RS)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an exact integer key and a vector of exact integers
; in strictly increasing order, returns the largest i such
; that element i of the vector is less than or equal to key,
; or -1 if key is less than every element of the vector.

(define (binary-search-of-vector key vec)

  ; Loop invariants:
  ; 0 <= i < j <= (vector-length vec)
  ; vec[i] <= key
  ; if j < (vector-length vec), then key < vec[j]

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             mid)
            ((<= (vector-ref vec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (vector-length vec)))
    (if (or (= hi 0) (< key (vector-ref vec 0)))
        -1
        (loop 0 hi))))

; Given an exact integer key and a vector of exact integers
; in strictly increasing order, returns the index of key,
; or returns #f if the key is not within the vector.

(define (binary-search key vec)

  ; Loop invariants:
  ; 0 <= i < j <= (vector-length vec)
  ; vec[i] <= key
  ; if j < (vector-length vec), then key < vec[j]

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             (if (= key (vector-ref vec mid))
                 mid
                 #f))
            ((<= (vector-ref vec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (vector-length vec)))
    (if (or (= hi 0) (< key (vector-ref vec 0)))
        #f
        (loop 0 hi))))

; Given an exact integer key and a bytevector of 16-bit unsigned
; big-endian integers in strictly increasing order,
; returns i such that elements (* 2 i) and (+ (* 2 i) 1)
; are equal to the key, or returns #f if the key is not found.

(define (binary-search-16bit key bvec)

  (define (bytevector-ref16 bvec i)
    (let ((i2 (+ i i)))
      (+ (* 256 (bytevector-u8-ref bvec i2))
         (bytevector-u8-ref bvec (+ i2 1)))))

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             (if (= (bytevector-ref16 bvec mid) key)
                 mid
                 #f))
            ((<= (bytevector-ref16 bvec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (quotient (bytevector-length bvec) 2)))
    (if (or (= hi 0) (< key (bytevector-ref16 bvec 0)))
        #f
        (loop 0 hi))))

;)
