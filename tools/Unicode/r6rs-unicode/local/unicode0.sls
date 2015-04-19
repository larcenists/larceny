; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright and permission notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file contains a few auxiliary procedures that are used
; by the reference implementation of the (rnrs unicode) library.
; None of the definitions in this file are exported as part of
; (rnrs unicode).
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

(library (local unicode0)
  (export

    binary-search-of-vector
    binary-search
    binary-search-16bit

    make-comparison-predicate)

  (import (rnrs base)
          (rnrs bytevectors))

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
    (let ((mid (div (+ i j) 2)))
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
    (let ((mid (div (+ i j) 2)))
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
    (let ((mid (div (+ i j) 2)))
      (cond ((= i mid)
             (if (= (bytevector-ref16 bvec mid) key)
                 mid
                 #f))
            ((<= (bytevector-ref16 bvec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (div (bytevector-length bvec) 2)))
    (if (or (= hi 0) (< key (bytevector-ref16 bvec 0)))
        #f
        (loop 0 hi))))

; Given a binary comparison predicate, returns a predicate
; that accepts two or more arguments.

(define (make-comparison-predicate binop)
  (letrec ((loop (lambda (first rest)
		   (cond ((null? rest)
			  #t)
			 ((binop first (car rest))
			  (loop (car rest) (cdr rest)))
			 (else
			  #f)))))
    (lambda (a b . rest)
      (if (null? rest)
	  (binop a b)
	  (and (binop a b)
	       (loop b rest))))))

)
