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
;;;     string-hash
;;;     string-ci-hash
;;;     find
;;;     filter
;;;     partition
;;;     fold-right
;;;     remove
;;;     cons*
;;;     list-sort
;;;     vector-sort
;;;     vector-sort!
;;;     string-hash
;;;     string-ci-hash
;;;     symbol-hash
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

        ;; R7RS Red Edition

        (except (scheme list)
                find filter partition fold-right remove cons*)
        (scheme vector)
        (except (scheme sort)
                list-sort vector-sort vector-sort!)
        (scheme set)
        (scheme charset)
        (except (scheme hash-table)
                string-hash string-ci-hash)
        (scheme ilist)
        (scheme rlist)
        (scheme ideque)
;       (scheme text)
        (scheme generator)
        (scheme lseq)
        (scheme stream)
        (scheme box)
        (scheme list-queue)
        (scheme ephemeron)
        (except (scheme comparator)
                string-hash string-ci-hash symbol-hash)
        )

(display "All libraries loaded with these name conflicts:\n")
(for-each (lambda (name) (display "    ") (write name) (newline))
          '(bytevector-copy!

            ;; new conflicts in R7RS Red Edition

            string-hash
            string-ci-hash
            find
            filter
            partition
            fold-right
            remove
            cons*
            list-sort
            vector-sort
            vector-sort!
            string-hash
            string-ci-hash
            symbol-hash))
(newline)
