; Support for intraprocedural value numbering.
;
; The set of available expressions is represented as a
; mutable abstract data type Available with these operations:
;
; make-available-table:                                    -> Available
; copy-available-table: Available                          -> Available
; available-expression: Available x Expr                   -> (symbol + {#f})
; available-variable:   Available x symbol                 -> Expr
; available-extend!:    Available x symbol x Expr x Killer ->
; available-kill!:      Available x Killer                 ->
;
; where Expr is of the form
;
; Expr  -->  W
;         |  (W_0 W_1 ...)
;
; W  -->  (quote K)
;      |  (begin I)
;
; and Killer is a fixnum, as defined later in this file.
;
; (make-available-table)
;     returns an empty table of available expressions.
; (copy-available-table available)
;     copies the given table.
; (available-expression available E)
;     returns the name of E if it is available in the table, else #f.
; (available-variable available T)
;     returns a constant or variable to use in place of T, else #f.
; (available-extend! available T E K)
;     adds the binding (T E) to the table, with Killer K.
;     If E is a variable and this binding is never killed, then copy
;         propagation will replace uses of T by uses of E; otherwise
;         commoning will replace uses of E by uses of T, until the
;         binding is killed.
; (available-kill! available K)
;     removes all bindings whose Killer intersects K.
;
; (available-extend! available T E K) is very fast if the previous
; operation on the table was (available-expression available E).

; Implementation.
;
; Quick and dirty.
; The available expressions are represented as a vector of 2 association
; lists.  The first list is used for common subexpression elimination,
; and the second is used for copy and constant propagation.
;
; Each element of the first list is a binding of
; a symbol T to an expression E, with killer K,
; represented by the list (E T K).
;
; Each element of the second list is a binding of
; a symbol T to an expression E, with killer K,
; represented by the list (T E K).
; The expression E will be a constant or variable.

(define (make-available-table)
  (vector '() '()))

(define (copy-available-table available)
  (vector (vector-ref available 0)
          (vector-ref available 1)))

(define (available-expression available E)
  (let ((binding (assoc E (vector-ref available 0))))
    (if binding
        (cadr binding)
        #f)))

(define (available-variable available T)
  (let ((binding (assq T (vector-ref available 1))))
    (if binding
        (cadr binding)
        #f)))

(define (available-extend! available T E K)
  (cond ((constant? E)
         (vector-set! available
                      1
                      (cons (list T E K)
                            (vector-ref available 1))))
        ((and (variable? E)
              (eq? K available:killer:none))
         (vector-set! available
                      1
                      (cons (list T E K)
                            (vector-ref available 1))))
        (else
         (vector-set! available
                      0
                      (cons (list E T K)
                            (vector-ref available 0))))))

(define (available-kill! available K)
  (vector-set! available
               0
               (filter (lambda (binding)
                         (zero?
                          (logand K
                                  (caddr binding))))
                       (vector-ref available 0)))
  (vector-set! available
               1
               (filter (lambda (binding)
                         (zero?
                          (logand K
                                  (caddr binding))))
                       (vector-ref available 1))))

(define (available-intersect! available0 available1 available2)
  (vector-set! available0
               0
               (intersection (vector-ref available1 0)
                             (vector-ref available2 0)))
  (vector-set! available0
               1
               (intersection (vector-ref available1 1)
                             (vector-ref available2 1))))

; The Killer concrete data type, represented as a fixnum.
;
; The set of side effects that can kill an available expression
; are a subset of
;
; assignments to global variables
; uses of SET-CAR!
; uses of SET-CDR!
; uses of STRING-SET!
; uses of VECTOR-SET!
;
; This list is not complete.  If we were trying to perform common
; subexpression elimination on calls to PEEK-CHAR, for example,
; then those calls would be killed by reads.

(define available:killer:globals   2)
(define available:killer:car       4)
(define available:killer:cdr       8)
(define available:killer:string   16) ; also bytevectors etc
(define available:killer:vector   32) ; also structures etc
(define available:killer:cell     64)
(define available:killer:io      128)
(define available:killer:none      0) ; none of the above
(define available:killer:all    1022) ; all of the above

(define available:killer:immortal  0) ; never killed
(define available:killer:dead   1023) ; never available



(define (available:killer-combine k1 k2)
  (logior k1 k2))
