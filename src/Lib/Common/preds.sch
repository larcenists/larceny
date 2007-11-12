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
;;; Copyright 2006 William D Clinger.
;;;
;;; Please do not copy this software.  It is in alpha test form.
;;; A better version will be distributed at some later date.
;;;
;;; Last modified 7 November 2007
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Uses R6RS hashtables.  Uses only:
;;;
;;; make-eq-hashtable
;;; hashtable-ref
;;; hashtable-set!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EQUIV?
;;;
;;; EQUIV? is a version of EQUAL? that terminates on all arguments.
;;;
;;; The basic idea of the algorithm is presented in
;;;
;;; J E Hopcroft and R M Karp.  A Linear Algorithm for
;;; Testing Equivalence of Finite Automata.
;;; Cornell University Technical Report 71-114,
;;; December 1971.
;;; http://techreports.library.cornell.edu:8081/Dienst/UI/1.0/Display/cul.cs/TR71-114
;;;
;;; The algorithm uses FIND and MERGE operations, which
;;; roughly correspond to done? and equate! in the code below.
;;; The algorithm maintains a stack of comparisons to do,
;;; and a set of equivalences that would be implied by the
;;; comparisons yet to be done.
;;;
;;; When comparing objects x and y whose equality cannot be
;;; determined without recursion, the algorithm pushes all
;;; the recursive subgoals onto the stack, and merges the
;;; equivalence classes for x and y.  If any of the subgoals
;;; involve comparing x and y, the algorithm will notice
;;; that they are in the same equivalence class and will
;;; avoid circularity by assuming x and y are equal.
;;; If all of the subgoals succeed, then x and y really are
;;; equal, so the algorithm is correct.
;;;
;;; If the hash tables give amortized constant-time lookup on
;;; object identity, then this algorithm could be made to run
;;; in O(n) time, where n is the number of nodes in the larger
;;; of the two structures being compared.
;;;
;;; If implemented in portable R5RS Scheme, the algorithm
;;; should still run in O(n^2) time, or close to it.
;;;
;;; This implementation uses two techniques to reduce the
;;; cost of the algorithm for common special cases:
;;;
;;; It starts out by trying the traditional recursive algorithm
;;; to bounded depth.
;;; It handles easy cases specially.

; How long should we try the traditional recursive algorithm
; before switching to the terminating algorithm?

(define equal:bound-on-recursion 100000)

(define (equal? x y)

  ; The traditional recursive algorithm, with bounded recursion.
  ; Returns #f or an exact integer n.
  ; If n > 0, then x and y are equal and the comparison involved
  ; bound - n recursive calls.
  ; If n <= 0, then the algorithm terminated before
  ; it could determine whether x and y are equal.

  (define (equal? x y bound)
    (cond ((eq? x y)
           bound)
          ((<= bound 0)
           bound)
          ((and (pair? x) (pair? y))
           (if (eq? (car x) (car y))
               (equal? (cdr x) (cdr y) (- bound 1))
               (let ((result (equal? (cdr x) (cdr y) (- bound 1))))
                 (if result
                     (equal? (car x) (car y) result)
                     #f))))
          ((and (vector? x) (vector? y))
           (let ((nx (vector-length x))
                 (ny (vector-length y)))
             (if (= nx ny)
                 (let loop ((i 0)
                            (bound (- bound 1)))
                   (if (< i nx)
                       (let ((result (equal? (vector-ref x i)
                                             (vector-ref y i)
                                             bound)))
                         (if result
                             (loop (+ i 1) result)
                             #f))
                       bound))
                 #f)))
          ((and (bytevector? x) (bytevector? y))
           (if (bytevector=? x y) bound #f))
          ((and (string? x) (string? y))
           (if (string=? x y) bound #f))
          ((eqv? x y)
           bound)
          (else #f)))

  ; Returns #t iff x and y would have the same (possibly infinite)
  ; printed representation.  Always terminates.

  (define (equiv? x y)
    (let ((done (initial-equivalences)))

      ; done is a hash table that maps objects to their
      ; equivalence classes.
      ;
      ; Algorithmic invariant:  If all of the comparisons that
      ; are in progress (pushed onto the control stack) come out
      ; equal, then all of the equivalences in done are correct.
      ;
      ; Invariant of this implementation:  The equivalence classes
      ; omit easy cases, which are defined as cases in which eqv?
      ; always returns the correct answer.  The equivalence classes
      ; also omit strings, because strings can be compared without
      ; risk of circularity.
      ;
      ; Invariant of this prototype:  The equivalence classes include
      ; only pairs and vectors.  If records or other things are to be
      ; compared recursively, then they should be added to done.
      ;
      ; Without constant-time lookups, it is important to keep
      ; done as small as possible.  This implementation takes
      ; advantage of several common cases for which it is not
      ; necessary to keep track of a node's equivalence class.

      (define (equiv? x y)
        ;(step x y done)
        (cond ((eqv? x y)
               #t)
              ((and (pair? x) (pair? y))
               (let ((x1 (car x))
                     (y1 (car y))
                     (x2 (cdr x))
                     (y2 (cdr y)))
                 (cond ((done? x y done)
                        #t)
                       ((eqv? x1 y1)
                        (equate! x y done)
                        (equiv? x2 y2))
                       ((eqv? x2 y2)
                        (equate! x y done)
                        (equiv? x1 y1))
                       ((easy? x1 y1)
                        #f)
                       ((easy? x2 y2)
                        #f)
                       (else
                        (equate! x y done)
                        (and (equiv? x1 y1)
                             (equiv? x2 y2))))))
              ((and (vector? x) (vector? y))
               (let ((n (vector-length x)))
                 (if (= n (vector-length y))
                     (if (done? x y done)
                         #t
                         (begin (equate! x y done)
                                (vector-equiv? x y n 0)))
                     #f)))
              ((and (bytevector? x) (bytevector? y))
               (bytevector=? x y))
              ((and (string? x) (string? y))
               (string=? x y))
              (else #f)))

      ; Like equiv? above, except x and y are known to be vectors,
      ; n is the length of both, and i is the first index that has
      ; not yet been pushed onto the todo set.
    
      (define (vector-equiv? x y n i)
        (if (< i n)
            (let ((xi (vector-ref x i))
                  (yi (vector-ref y i)))
              (if (easy? xi yi)
                  (if (eqv? xi yi)
                      (vector-equiv? x y n (+ i 1))
                      #f)
                  (and (equiv? xi yi)
                       (vector-equiv? x y n (+ i 1)))))
            #t))

      (equiv? x y)))

  ; A comparison is easy if eqv? returns the right answer.

  (define (easy? x y)
    (cond ((eqv? x y)
           #t)
          ((pair? x)
           (not (pair? y)))
          ((pair? y)
           #t)
          ((vector? x)
           (not (vector? y)))
          ((vector? y)
           #t)
          ((vector? x)
           (not (vector? y)))
          ((vector? y)
           #t)
          ((bytevector? x)
           (not (bytevector? y)))
          ((bytevector? y)
           #t)
          ((not (string? x))
           #t)
          ((not (string? y))
           #t)
          (else #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; Tables mapping objects to their equivalence classes.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; FIXME:  Equivalence classes are represented as lists,
  ; which means they can't be merged in constant time.

  (define (initial-equivalences)
    (make-eq-hashtable))

  ; Are x and y equivalent according to the table?

  (define (done? x y table)
    (memq x (hashtable-ref table y '())))

  ; Merge the equivalence classes of x and y in the table,
  ; and return the table.  Changes the table.

  (define (equate! x y table)
    (let ((xclass (hashtable-ref table x '()))
          (yclass (hashtable-ref table y '())))
      (cond ((and (null? xclass) (null? yclass))
             (let ((class (list x y)))
               (hashtable-set! table x class)
               (hashtable-set! table y class)))
            ((null? xclass)
             (let ((class0 (cons x (cdr yclass))))
               (set-cdr! yclass class0)
               (hashtable-set! table x yclass)))
            ((null? yclass)
             (let ((class0 (cons y (cdr xclass))))
               (set-cdr! xclass class0)
               (hashtable-set! table y xclass)))
            ((eq? xclass yclass)
             #t)
            ((memq x yclass)
             #t)
            (else
             (let ((class0 (append (cdr xclass) yclass)))
               (set-cdr! xclass class0)
               (set-car! yclass (car xclass))
               (set-cdr! yclass class0)))))
    table)

  (let ((result (equal? x y equal:bound-on-recursion)))
    (if result
        (if (> result 0)
            #t
            (equiv? x y))
        #f)))

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
