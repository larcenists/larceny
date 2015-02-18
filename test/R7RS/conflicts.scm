;;; Tests for name conflicts among standard libraries.
;;;
;;; Known conflicts:
;;;
;;;     bytevector-copy!    (unresolvable; requires renaming)
;;;
;;;     define-record-type  (resolved by allowing both R7RS and R6RS syntax)
;;;     error               (resolved by heuristic overloading)
;;;     map                 (resolved by using R7RS extended semantics)
;;;     for-each            (resolved by using R7RS extended semantics)
;;;     finite?             (resolved by using R7RS extended semantics)
;;;     infinite?           (resolved by using R7RS extended semantics)
;;;     nan?                (resolved by using R7RS extended semantics)
;;;     string-copy         (resolved by using R7RS extended semantics)
;;;     string-fill!        (resolved by using R7RS extended semantics)
;;;     vector-copy         (resolved by using R7RS extended semantics)
;;;     bytevector-copy     (resolved by using R7RS extended semantics)
;;;     utf8->string        (resolved by using R7RS extended semantics)
;;;     string->utf8        (resolved by using R7RS extended semantics)
;;;     textual-port?       (resolved by using R7RS extended semantics)
;;;     binary-port?        (resolved by using R7RS extended semantics)
;;;
;;; Possible conflicts:
;;;
;;;     

(import (except (rnrs) bytevector-copy!)
        (rnrs eval)
        (rnrs mutable-pairs)
        (rnrs mutable-strings)
        (rnrs r5rs)

        (scheme base)
        (scheme case-lambda)
        (scheme char)
        (scheme complex)
        (scheme cxr)
        (scheme eval)
        (scheme file)
        (scheme inexact)
        (scheme lazy)
        (scheme load)
        (scheme process-context)
        (scheme read) 
        (scheme repl)
        (scheme time)
        (scheme write)
        (scheme r5rs)
        )

(display "All libraries loaded with these name conflicts:\n")
(for-each (lambda (name) (display "    ") (write name) (newline))
          '(bytevector-copy!))
(newline)
