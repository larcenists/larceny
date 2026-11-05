; Copyright Lightship Software, Incorporated.
; 
; $Id$
;
; Larceny library -- general predicates.

($$trace "preds")
 
(define (atom? x) (not (pair? x)))

(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

; FIXME: old version of equal?, for reference.

;(define (equal? x y)
;  (cond ((eqv? x y) #t)
;	((and (pair? x) (pair? y))
;	 (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
;	((and (string? x) (string? y))
;	 (string=? x y))
;	((and (vector? x) (vector? y))
;	 (vector-equal? x y))
;	((and (bytevector? x) (bytevector? y))
;	 (bytevector-equal? x y))
;	((and (structure? x) (structure? y))
;	 ((structure-comparator) x y))
;	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQUAL?
;;;
;;; Terminates on all arguments, including circular structures, by the
;;; interleaved algorithm of Michael D Adams and R Kent Dybvig
;;; ("Efficient Nondestructive Equality Checking for Trees and Graphs",
;;; ICFP 2008), as in Chez Scheme:
;;;
;;; The comparison starts in fast mode, the traditional recursive
;;; algorithm, with a budget of equal:fast-nodes pairs and vectors.
;;; When the budget runs out it compares the next equal:slow-nodes pairs
;;; and vectors in slow mode, which keeps a union-find structure of the
;;; objects known or assumed to be equal: comparing x and y when they
;;; are already in one class succeeds, and otherwise their classes are
;;; merged before their elements are compared.  (That is sound because
;;; any difference anywhere makes the answer #f.)  Slow mode goes on
;;; while it finds already equated objects; after equal:slow-nodes new
;;; ones in a row it returns to fast mode with a new budget.  Circular and shared structure thus
;;; costs little, while trees and long lists are compared mostly in
;;; fast mode, and small ones without a table at all.
;;;
;;; This replaces an algorithm (Will Clinger, 2006, after Hopcroft and
;;; Karp) that tried the recursive algorithm for up to 5000000 steps
;;; before switching for good to equivalence classes kept as lists, so
;;; that comparing circular or heavily shared structures took the 5000000
;;; steps every time.

; Chez uses budgets of a few hundred and a few dozen nodes; these are
; larger because hashtable operations cost much more than a fast step
; here.  (The equal benchmark, test/Benchmarking/R6RS, takes 0.64 s of
; comparisons with these, against 2.9 s before: 4 ms instead of 1625 for
; circular lists, 118 instead of 1000 for shared trees, 212 and 237
; instead of 110 and 140 for lists of shared trees and deep trees.)

(define equal:fast-nodes 50000)
(define equal:slow-nodes 1000)

(define (equal? x y)

  (define table #f)                       ; object -> class node

  ; A class node is a vector of one element, the node's parent, or #f
  ; in the representative of the class.

  (define (find node)
    (let loop ((root node))
      (let ((parent (vector-ref root 0)))
        (if parent
            (loop parent)
            (let compress ((node node))  ; path compression
              (let ((parent (vector-ref node 0)))
                (if (and parent (not (eq? parent root)))
                    (begin (vector-set! node 0 root)
                           (compress parent))
                    root)))))))

  (define (node-of x)
    (or (hashtable-ref table x #f)
        (let ((node (vector #f)))
          (hashtable-set! table x node)
          node)))

  ; Returns #t if x and y are in one class, and otherwise merges their
  ; classes and returns #f.

  (define (same-class! x y)
    (if (not table)
        (set! table (make-eq-hashtable)))
    (let ((rx (find (node-of x)))
          (ry (find (node-of y))))
      (or (eq? rx ry)
          (begin (vector-set! rx 0 ry)
                 #f))))


					; Compares x and y with budget k: fast mode while k > 0, slow mode
  ; for k from 0 down to (- equal:slow-nodes).  Returns #f if they
  ; differ, and otherwise the budget left.  In slow mode, a pair or
  ; vector already equated to the other succeeds at once, and the
  ; budget goes back to 0: slow mode lasts while it keeps finding
  ; shared structure, and ends only after equal:slow-nodes new nodes in
  ; a row.  (Otherwise fast mode would explore shared structure as if
  ; it were a tree.)

  (define (e? x y k)
    (cond ((eq? x y)
           k)
          ((and (pair? x) (pair? y))
           (if (and (<= k 0) (same-class! x y))
               0
               (let ((k (e? (car x) (car y) (next k))))
                 (and k (e? (cdr x) (cdr y) k)))))
          ((and (vector? x) (vector? y))
           (let ((n (vector-length x)))
             (and (= n (vector-length y))
                  (if (and (<= k 0) (same-class! x y))
                      0
                      (let loop ((i 0) (k (next k)))
                        (if (< i n)
                            (let ((k (e? (vector-ref x i) (vector-ref y i) k)))
                              (and k (loop (+ i 1) k)))
                            k))))))
          ((and (bytevector? x) (bytevector? y))
           (and (bytevector=? x y) k))
          ((and (string? x) (string? y))
           (and (string=? x y) k))
          ((and (text? x) (text? y))
           (and (textual=? x y) k))
          ((and (numeric-vector? x) (numeric-vector? y))
           (and (numeric-vector=? x y) k))
          ((eqv? x y)
           k)
          (else #f)))

  ; The budget after a pair or vector: in slow mode, back to fast mode
  ; after equal:slow-nodes of them.

  (define (next k)
    (cond ((> k 0) (- k 1))
          ((<= k (- equal:slow-nodes)) equal:fast-nodes)
          (else (- k 1))))

  (if (e? x y equal:fast-nodes) #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2007 William D Clinger
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (boolean=? x y . rest)
  (cond ((or (not (boolean? x)) (not (boolean? y)))
         (assertion-violation 'boolean=? "illegal arguments" x y))
        ((null? rest)
         (eq? x y))
        (else
         (and (eq? x y) (apply boolean=? y rest)))))

(define (symbol=? x y . rest)
  (cond ((or (not (symbol? x)) (not (symbol? y)))
         (assertion-violation 'symbol=? "illegal arguments" x y))
        ((null? rest)
         (eq? x y))
        (else
         (and (eq? x y) (apply symbol=? y rest)))))
 
; eof
