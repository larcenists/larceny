;;; Tests for name conflicts between SRFIs and R7RS libraries.
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



(import ;(except (rnrs) bytevector-copy!)
        ;(rnrs eval)
        ;(rnrs mutable-pairs)
        ;(rnrs mutable-strings)
        ;(rnrs r5rs)

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

        (except (srfi 1) assoc for-each map list-copy make-list member)
        (srfi 2)
        (except (srfi 5) let)
        (srfi 6)
        (srfi 8)
        (srfi 9)
        (srfi 11)
        (except (srfi 13)
                string-copy string-copy! string-downcase string-upcase
                string-fill! string-for-each string-map string->list)
        (srfi 14)
        (srfi 16)
        (except (srfi 17) set!)
        (srfi 19)
        (except (srfi 23) error)
        (srfi 25)
        (srfi 26)
        (srfi 27)
        (except (srfi 28) format)
        (except (srfi 29) format)
        (srfi 31)
        (srfi 38)
        (except (srfi 39) make-parameter parameterize)
        (srfi 41)
        (srfi 42)
        (except (srfi 43)
                list->vector vector->list
                vector-copy vector-copy!
                vector-for-each vector-map
                vector-append vector-fill!)
        (srfi 45 lazy) ; FIXME
        (srfi 48)
        (srfi 51)
        (srfi 54)
        (srfi 59)
        (srfi 60)
        (except (srfi 61) cond)
        (except (srfi 63) equal? array? make-array array-rank array-ref array-set!)
        (srfi 64)
        (srfi 66)
        (except (srfi 67) string-compare string-compare-ci)
        (except (srfi 69) string-hash)
        (except (srfi 71) let let* letrec)
        (srfi 74)
        (srfi 78)
        (except (srfi 87) case)
        (srfi 95)
        (srfi 98)
        (srfi 99)
        (except (srfi 101) cons list make-list pair? null? list? car cdr
                caar cadr cdar cddr
                caaar caadr caddr cadar cdaar cdadr cdddr cddar                
                caaaar caaadr caaddr caadar cadaar cadadr cadddr caddar
                cdaaar cdaadr cdaddr cdadar cddaar cddadr cddddr cdddar
                list-ref list-tail length append reverse
                for-each map quote)
        (srfi 111)
        (srfi 112)
        (srfi 113)
        (except (srfi 114) if3 if=? if<? if>? if<=? if>=? if-not=? =? <? >? <=? >=?)
        (srfi 115)
        (srfi 116)
        )

(display "All libraries loaded with these name conflicts:\n")
(for-each (lambda (name) (display "    ") (write name) (newline))
          '((assoc for-each map list-copy make-list member (srfi 1))
            (let (srfi 5))
            (string-copy string-copy! string-downcase string-upcase
             string-fill! string-for-each string-map string->list
             (srfi 13))
            (set! (srfi 17))
            (error (srfi 23))
            (format (srfi 28 29))
            (make-parameter parameterize (srfi 39))
            (list->vector vector->list
             vector-copy vector-copy!
             vector-for-each vector-map
             vector-append vector-fill! (srfi 43))
            (cond (srfi 61))
            (equal? array? make-array array-rank array-ref array-set!
             (srfi 25 63))
            (string-hash (srfi 13 69))
            (string-compare string-compare-ci (srfi 13 67))
            (let let* letrec (srfi 71))
            (case (srfi 87))
            (cons list make-list pair? null? list? car cdr
             caar cadr cdar cddr
             caaar caadr caddr cadar cdaar cdadr cdddr cddar                
             caaaar caaadr caaddr caadar cadaar cadadr cadddr caddar
             cdaaar cdaadr cdaddr cdadar cddaar cddadr cddddr cdddar
             list-ref list-tail length append reverse
             for-each map quote (srfi 101))
            (if3 if=? if<? if>? if<=? if>=? if-not=? =? <? >? <=? >=?
             (srfi 67 114))))
(newline)
