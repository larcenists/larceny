;;; Tests for name conflicts among standard libraries.
;;;
;;; Known conflicts:
;;;
;;;     map                 (resolved by mode-dependent behavior)
;;;     for-each            (resolved by mode-dependent behavior)
;;;     string-fill!        (resolved by using R7RS extended semantics)
;;;     define-record-type  (resolved by allowing both R7RS and R6RS syntax)
;;;
;;; Possible conflicts:
;;;
;;;     

(import (rnrs)
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
       ;(scheme lazy)
        (scheme load)
        (scheme process-context)
        (scheme read) 
        (scheme repl)
        (scheme time)
        (scheme write)
       ;(scheme r5rs)
        )

(display "All libraries loaded without name conflicts.")
(newline)
