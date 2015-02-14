;;; Tests for name conflicts among standard libraries.
;;;
;;; Known conflicts:
;;;
;;;     bytevector-copy!    (unresolvable; requires renaming)
;;;
;;;     define-record-type  (resolved by allowing both R7RS and R6RS syntax)
;;;     error               (resolved by heuristic overloading)
;;;     map                 (resolved by mode-dependent behavior)
;;;     for-each            (resolved by mode-dependent behavior)
;;;     string-copy         (resolved by using R7RS extended semantics)
;;;     vector-copy         (resolved by using R7RS extended semantics)
;;;     bytevector-copy     (resolved by using R7RS extended semantics)
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

        (except (scheme base)
                string-fill!)
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
