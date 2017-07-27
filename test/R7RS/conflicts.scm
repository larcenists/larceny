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
;;;     remove              (unresolvable; requires renaming)
;;;     string-hash         (deprecated version takes optional argument)
;;;     string-ci-hash      (deprecated version takes optional argument)
;;;
;;;     vector-sort         (resolved by using R7RS extended semantics)
;;;     vector-sort!        (resolved by using R7RS extended semantics)
;;;
;;; Possible conflicts:
;;;
;;;     

(import (except (rnrs) bytevector-copy! remove)
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

        ;; R7RS Red Edition

        (scheme list)
        (scheme vector)
        (scheme sort)
        (scheme set)
        (scheme charset)
        (except (scheme hash-table)
                string-hash      ; incompatible because they must accept
                string-ci-hash)  ; an optional second argument
        (scheme ilist)
        (scheme rlist)
        (scheme ideque)
        (scheme text)
        (scheme generator)
        (scheme lseq)
        (scheme stream)
        (scheme box)
        (scheme list-queue)
        (scheme ephemeron)
        (scheme comparator)
        )

(display "All libraries loaded with these name conflicts:\n")
(for-each (lambda (name) (display "    ") (write name) (newline))
          '(bytevector-copy!

            ;; new conflicts in R7RS Red Edition

            remove
            string-hash
            string-ci-hash))
(newline)
