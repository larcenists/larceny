; Test suite for SRFI 64
;
; $Id$
;
; Two tests have been commented out because they reveal a bug
; in the reference implementation of SRFI 25 for which no fix
; appears to have been published.  That bug is irrelevant to
; SRFI 64.  Search for [Larceny].

(import (rnrs base)
        (rnrs io simple)                ; [Larceny] just for the last two lines
        (srfi :25 multi-dimensional-arrays)
        (srfi :64 testing))

;(test-begin "arrays" 169) ; [Larceny]
(test-begin "arrays" 167)

;;; array test
;;; 2001 Jussi Piitulainen
;;; 2002 Per Bothner modified to fit Kawa testing framework.
;;; 2006 Per Bothner modified to fit SRFI-64 testing framework.

;;; Simple tests

(test-equal "shape" #t
      (and (array? (shape))
	   (array? (shape -1 -1))
	   (array? (shape -1 0))
	   (array? (shape -1 1))
	   (array? (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8))))

(test-equal "make-array" #t
      (and (array? (make-array (shape)))
	   (array? (make-array (shape) *))
	   (array? (make-array (shape -1 -1)))
	   (array? (make-array (shape -1 -1) *))
	   (array? (make-array (shape -1 1)))
	   (array? (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *))))

(test-equal "array" #t
      (and (array? (array (shape) *))
	   (array? (array (shape -1 -1)))
	   (array? (array (shape -1 1) * *))
	   (array? (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *))))

(test-equal 2 (array-rank (shape)))
(test-equal 2 (array-rank (shape -1 -1)))
(test-equal 2 (array-rank (shape -1 1)))
(test-equal 2 (array-rank (shape 1 2 3 4 5 6 7 8)))

(test-equal 0 (array-rank (make-array (shape))))
(test-equal 1 (array-rank (make-array (shape -1 -1))))
(test-equal 1 (array-rank (make-array (shape -1 1))))
(test-equal 4 (array-rank (make-array (shape 1 2 3 4 5 6 7 8))))

(test-equal 0 (array-rank (array (shape) *)))
(test-equal 1 (array-rank (array (shape -1 -1))))
(test-equal 1 (array-rank (array (shape -1 1) * *)))
(test-equal 4 (array-rank (array (shape 1 2 3 4 5 6 7 8) *)))

(test-equal 0 (array-start (shape -1 -1) 0))
(test-equal 0 (array-start (shape -1 -1) 1))
(test-equal 0 (array-start (shape -1 1) 0))
(test-equal 0 (array-start (shape -1 1) 1))
(test-equal 0 (array-start (shape 1 2 3 4 5 6 7 8) 0))
(test-equal 0 (array-start (shape 1 2 3 4 5 6 7 8) 1))

(test-equal 1 (array-end (shape -1 -1) 0))
(test-equal 2 (array-end (shape -1 -1) 1))
(test-equal 1 (array-end (shape -1 1) 0))
(test-equal 2 (array-end (shape -1 1) 1))
(test-equal 4 (array-end (shape 1 2 3 4 5 6 7 8) 0))
(test-equal 2 (array-end (shape 1 2 3 4 5 6 7 8) 1))

(test-equal -1 (array-start (make-array (shape -1 -1)) 0))
(test-equal -1 (array-start (make-array (shape -1 1)) 0))
(test-equal 1 (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 0))
(test-equal 3 (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 1))
(test-equal 5 (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 2))
(test-equal 7 (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 3))

(test-equal -1 (array-end (make-array (shape -1 -1)) 0))
(test-equal 1 (array-end (make-array (shape -1 1)) 0))
(test-equal 2 (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 0))
(test-equal 4 (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 1))
(test-equal 6 (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 2))
(test-equal 8 (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 3))

;; array-start of array
(test-equal -1 (array-start (array (shape -1 -1)) 0))
(test-equal -1 (array-start (array (shape -1 1) * *) 0))
(test-equal 1 (array-start (array (shape 1 2 3 4 5 6 7 8) *) 0))
(test-equal 3 (array-start (array (shape 1 2 3 4 5 6 7 8) *) 1))
(test-equal 5 (array-start (array (shape 1 2 3 4 5 6 7 8) *) 2))
(test-equal 7 (array-start (array (shape 1 2 3 4 5 6 7 8) *) 3))

;; array-end of array
(test-equal -1 (array-end (array (shape -1 -1)) 0))
(test-equal 1 (array-end (array (shape -1 1) * *) 0))
(test-equal 2 (array-end (array (shape 1 2 3 4 5 6 7 8) *) 0))
(test-equal 4 (array-end (array (shape 1 2 3 4 5 6 7 8) *) 1))
(test-equal 6 (array-end (array (shape 1 2 3 4 5 6 7 8) *) 2))
(test-equal 8 (array-end (array (shape 1 2 3 4 5 6 7 8) *) 3))

;; array-ref of make-array with arguments
(test-equal 'a (array-ref (make-array (shape) 'a)))
(test-equal 'b (array-ref (make-array (shape -1 1) 'b) -1))
(test-equal 'c (array-ref (make-array (shape -1 1) 'c) 0))
(test-equal 'd (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) 1 3 5 7))

;; array-ref of make-array with vector
(test-equal 'a (array-ref (make-array (shape) 'a) '#()))
(test-equal 'b (array-ref (make-array (shape -1 1) 'b) '#(-1)))
(test-equal 'c (array-ref (make-array (shape -1 1) 'c) '#(0)))
(test-equal 'd (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) '#(1 3 5 7)))

;; array-ref of make-array with array
(test-equal 'a (array-ref (make-array (shape) 'a) (array (shape 0 0))))
(test-equal 'b (array-ref (make-array (shape -1 1) 'b) (array (shape 0 1) -1)))
(test-equal 'c (array-ref (make-array (shape -1 1) 'c) (array (shape 0 1) 0)))
(test-equal 'd (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
      (array (shape 0 4) 1 3 5 7)))

;; array-set! of make-array with arguments
(test-equal "set" 'a
      (let ((arr (make-array (shape) 'o)))
	(array-set! arr 'a)
	(array-ref arr)))
(let ((arr (make-array (shape -1 1) 'o)))
  (array-set! arr -1 'b)
  (array-set! arr 0 'c)
  (test-equal 'b (array-ref arr -1))
  (test-equal 'c (array-ref arr 0)))
(let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
  (array-set! arr 1 3 5 7 'd)
  (test-equal 'd (array-ref arr 1 3 5 7)))

;; array-set! of make-array with vector
(let ((arr (make-array (shape) 'o)))
  (array-set! arr '#() 'a)
  (test-equal 'a (array-ref arr)))
(let ((arr (make-array (shape -1 1) 'o)))
  (array-set! arr '#(-1) 'b)
  (array-set! arr '#(0) 'c)
  (test-equal 'b (array-ref arr -1))
  (test-equal 'c (array-ref arr 0)))
(let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
  (array-set! arr '#(1 3 5 7) 'd)
  (test-equal 'd (array-ref arr 1 3 5 7)))

;; array-set! of make-array with array
(let ((arr (make-array (shape) 'o)))
  (array-set! arr 'a)
  (test-equal 'a (array-ref arr)))
(let ((arr (make-array (shape -1 1) 'o)))
  (array-set! arr (array (shape 0 1) -1) 'b)
  (array-set! arr (array (shape 0 1) 0) 'c)
  (test-equal 'b (array-ref arr -1))
  (test-equal 'c (array-ref arr 0)))
(let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
  (array-set! arr (array (shape 0 4) 1 3 5 7) 'd)
  (test-equal 'd (array-ref arr 1 3 5 7)))

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

;; shared change
(let* ((org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f))
       (brk (share-array
	     org
	     (shape 2 4 1 3)
	     (lambda (r k)
	       (values
		(+ 6 (* 2 (- r 2)))
		(- k 1)))))
       (swp (share-array
	     org
	     (shape 3 5 5 7)
	     (lambda (r k)
	       (values
		(+ 7 (- r 3))
		(- 1 (- k 5))))))
       (box (share-array
	     swp
	     (shape 0 1 2 3 4 5 6 7 8 9)
	     (lambda _ (values 4 6))))
       (org-contents (lambda ()
		       (list (array-ref org 6 0) (array-ref org 6 1)
			     (array-ref org 7 0) (array-ref org 7 1)
			     (array-ref org 8 0) (array-ref org 8 1))))
       (brk-contents (lambda ()
		       (list (array-ref brk 2 1) (array-ref brk 2 2)
			     (array-ref brk 3 1) (array-ref brk 3 2))))
       (swp-contents (lambda ()
		       (list (array-ref swp 3 5) (array-ref swp 3 6)
			     (array-ref swp 4 5) (array-ref swp 4 6))))
       (box-contents (lambda ()
		       (list (array-ref box 0 2 4 6 8)))))
  (test-equal '(a b c d e f) (org-contents))
  (test-equal '(a b e f) (brk-contents))
  (test-equal '(d c f e) (swp-contents))
  (test-equal '(e) (box-contents))
  (array-set! org 6 0 'x)
  (test-equal '(x b c d e f) (org-contents))
  (test-equal '(x b e f) (brk-contents))
  (test-equal '(d c f e) (swp-contents))
  (test-equal '(e) (box-contents))
  (array-set! brk 3 1 'y)
  (test-equal '(x b c d y f) (org-contents))
  (test-equal '(x b y f) (brk-contents))
  (test-equal  '(d c f y) (swp-contents))
  (test-equal '(y) (box-contents))
  (array-set! swp 4 5 'z)
  (test-equal '(x b c d y z) (org-contents))
  (test-equal '(x b y z) (brk-contents))
  (test-equal '(d c z y) (swp-contents))
  (test-equal '(y) (box-contents))
  (array-set! box 0 2 4 6 8 'e)
  (test-equal '(x b c d e z) (org-contents))
  (test-equal '(x b e z) (brk-contents))
  (test-equal '(d c z e) (swp-contents))
  (test-equal '(e) (box-contents)))

;;; Check that arrays copy the shape specification

;; array-set! of shape
(let ((shp (shape 10 12)))
  (let ((arr (make-array shp))
	(ars (array shp * *))
	(art (share-array (make-array shp) shp (lambda (k) k))))
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (test-equal 2 (array-rank shp))
    (test-equal 0 (array-start shp 0))
    (test-equal 1 (array-end shp 0))
    (test-equal 0 (array-start shp 1))
    (test-equal 2 (array-end shp 1))
    (test-equal '? (array-ref shp 0 0))
    (test-equal '! (array-ref shp 0 1))
    (test-equal 1 (array-rank arr))
    (test-equal 10 (array-start arr 0))
    (test-equal 12 (array-end arr 0))
    (test-equal 1 (array-rank ars))
    (test-equal 10 (array-start ars 0))
    (test-equal 12 (array-end ars 0))
    (test-equal 1 (array-rank art))
    (test-equal 10 (array-start art 0))
    (test-equal 12 (array-end art 0))))

;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

;; array access with sharing index array
(let ((arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se))
      (ixn (array (shape 0 2 0 2) 4 6 5 4)))
  (let ((col0 (share-array
	       ixn
	       (shape 0 2)
	       (lambda (k) (values k 0))))
	(row0 (share-array
	       ixn
	       (shape 0 2)
	       (lambda (k) (values 0 k))))
	(wor1 (share-array
	       ixn
	       (shape 0 2)
	       (lambda (k) (values 1 (- 1 k)))))
	(cod (share-array
	      ixn
	      (shape 0 2)
	      (lambda (k)
		(case k
		  ((0) (values 1 0))
		  ((1) (values 0 1))))))
	(box (share-array
	      ixn
	      (shape 0 2)
	      (lambda (k) (values 1 0)))))
    (test-equal 'nw (array-ref arr col0))
    (test-equal 'ne (array-ref arr row0))
    (test-equal 'nw (array-ref arr wor1))
    (test-equal 'se (array-ref arr cod))
    (test-equal 'sw (array-ref arr box))
    (array-set! arr col0 'ul)
    (array-set! arr row0 'ur)
    (array-set! arr cod 'lr)
    (array-set! arr box 'll)
    (test-equal 'ul (array-ref arr 4 5))
    (test-equal 'ur (array-ref arr 4 6))
    (test-equal 'll (array-ref arr 5 5))
    (test-equal 'lr (array-ref arr 5 6))
    (array-set! arr wor1 'xx)
    (test-equal 'xx (array-ref arr 4 5))))

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1 
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

;; sharing shape array
(let ((arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13)))
  (let ((shp (share-array
	      arr
	      (shape 0 2 0 2)
	      (lambda (r k) (values (+ r 1) (+ k 1)))))
	(shq (share-array
	      arr
	      (shape 0 2 0 2)
	      (lambda (r k) (values (+ r 1) (* 2 (+ 1 k))))))
	(shr (share-array
	      arr
	      (shape 0 4 0 2)
	      (lambda (r k) (values (- 2 k) (+ r 1)))))
	(shs (share-array
	      arr
	      (shape 0 2 0 2)
	      (lambda (r k) (values 2 3)))))
    (let ((arr-p (make-array shp)))
      (test-equal  2 (array-rank arr-p))
      (test-equal 10 (array-start arr-p 0))
      (test-equal 12 (array-end arr-p 0))
      (test-equal 10 (array-start arr-p 1))
      (test-equal 11 (array-end arr-p 1)))
    (let ((arr-q (array shq * * * *  * * * *  * * * *  * * * *)))
      (test-equal 2 (array-rank arr-q))
      (test-equal 12 (array-start arr-q 0))
      (test-equal 20 (array-end arr-q 0))
      (test-equal 11 (array-start arr-q 1))
      (test-equal 13 (array-end arr-q 1)))
    (let ((arr-r (share-array
		  (array (shape) *)
		  shr
		  (lambda _ (values)))))
      (test-equal 4 (array-rank arr-r))
      (test-equal 10 (array-start arr-r 0))
      (test-equal 10 (array-end arr-r 0))
      (test-equal 11 (array-start arr-r 1))
      (test-equal 12 (array-end arr-r 1))
      (test-equal 12 (array-start arr-r 2))
      (test-equal 16 (array-end arr-r 2))
      (test-equal 13 (array-start arr-r 3))
      (test-equal 20 (array-end arr-r 3)))
    (let ((arr-s (make-array shs)))
      (test-equal 2 (array-rank arr-s))
      (test-equal 12 (array-start arr-s 0))
      (test-equal 12 (array-end arr-s 0))
      (test-equal 12 (array-start arr-s 1))
      (test-equal 12 (array-end arr-s 1) 12))
    (let ((arr-s (make-array shs)))
      (test-equal 2 (array-rank arr-s))
      (test-equal 12 (array-start arr-s 0))
      (test-equal 12 (array-end arr-s 0))
      (test-equal 12 (array-start arr-s 1))
      (test-equal 12 (array-end arr-s 1) 12))))

;; sharing with sharing subshape
(let ((super (array (shape 4 7 4 7)
                    1 * *
                    * 2 *
                    * * 3))
      (subshape (share-array
                 (array (shape 0 2 0 3)
                        * 4 *
                        * 7 *)
                 (shape 0 1 0 2)
                 (lambda (r k)
                   (values k 1)))))
  (let ((sub (share-array super subshape (lambda (k) (values k k)))))
    ;(array-equal? subshape (shape 4 7))
    (test-equal 2 (array-rank subshape))
    (test-equal 0 (array-start subshape 0))
    (test-equal 1 (array-end subshape 0))
    (test-equal 0 (array-start subshape 1))
    (test-equal 2 (array-end subshape 1))
    (test-equal 4 (array-ref subshape 0 0))
    (test-equal 7 (array-ref subshape 0 1))
    ;(array-equal? sub (array (shape 4 7) 1 2 3))
    (test-equal 1 (array-rank sub))
    (test-equal 4 (array-start sub 0))
    (test-equal 7 (array-end sub 0))
    (test-equal 1 (array-ref sub 4))
    (test-equal 2 (array-ref sub 5))
    (test-equal 3 (array-ref sub 6))))

;; Bug reported by Chris Dean <ctdean@mercedsystems.com>

(define a-2-9 (make-array (shape 0 2 0 9)))
(array-set! a-2-9 1 3 'e)
(test-equal 'e (array-ref a-2-9 1 3))

;; Savannah [bug #4310] share-array edge case.  All these tests should
;; return ok without an error or IndexOutOfBoundsException
(define (make-simple-affine ndims hibound)
  (lambda (i)
    (if (> i hibound) 
        (error "index out of bounds" i hibound))
    (apply values (vector->list (make-vector ndims i)))))

(define four-dee-array (array (shape 0 2 0 2 0 2 0 2)
			      'a 'b 'c 'd 'e 'f 'g 'h
			      'i 'j 'k 'l 'm 'n 'o 'p))
(define four-dee-lil-array (make-array (shape 0 1 0 1 0 1 0 1) 'ok))

; [Larceny] comments out this test
'
(test-equal 'ok
	    (array-ref
	     (share-array four-dee-lil-array (shape 0 1) (make-simple-affine 4 0))
	     0))
; [Larceny] comments out this test
'
(test-equal 'a
	    (array-ref
	     (share-array four-dee-array (shape 0 1) (make-simple-affine 4 0))
	     0))
(test-equal 'a
	    (array-ref
	     (share-array four-dee-array (shape 0 2) (make-simple-affine 4 1))
	     0))
(test-equal
 '(a p)
 (map
  (lambda (i)
    (array-ref
     (share-array four-dee-array (shape 0 2) (make-simple-affine 4 1))
     i))
  '(0 1)))
(test-equal 'p
	    (array-ref
	     (share-array four-dee-array (shape 1 2) (make-simple-affine 4 1))
	     1))

(test-end)

(display "Done.") ; [Larceny]
(newline)         ; [Larceny]
