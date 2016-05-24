;;; Tests for name conflicts between SRFIs and R7RS/R6RS libraries.

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

        (srfi 1)
        (srfi 2)
        (except (srfi 5) let)
        (srfi 6)
        (srfi 8)
        (srfi 9)
        (srfi 11)
        (except (srfi 13) string-downcase string-upcase string-titlecase
                          string-for-each string-map string-hash)
        (srfi 14)
        (srfi 16)
        (except (srfi 17) set!)
        (srfi 19)
        (srfi 23)
        (srfi 25)
        (srfi 26)
        (srfi 27)
        (except (srfi 28) format)
        (except (srfi 29) format)
        (srfi 31)
        (srfi 38)
        (srfi 39)
        (srfi 41)
        (srfi 42)
        (except (srfi 43)
                list->vector
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
        (except (srfi 63)
                equal? array? make-array array-rank array-ref array-set!)
        (srfi 64)
        (srfi 66)
        (except (srfi 67) string-compare string-compare-ci)
        (except (srfi 69) string-hash string-ci-hash)
        (except (srfi 71) let let* letrec)
        (except (srfi 74) endianness)
        (srfi 78)
        (srfi 87)
        (srfi 95)
        (srfi 98)
        (srfi 99)
        (except (srfi 101)
                cons list make-list pair? null? list? car cdr
                caar cadr cdar cddr
                caaar caadr caddr cadar cdaar cdadr cdddr cddar                
                caaaar caaadr caaddr caadar cadaar cadadr cadddr caddar
                cdaaar cdaadr cdaddr cdadar cddaar cddadr cddddr cdddar
                list-ref list-tail length append reverse
                for-each map quote)
        (srfi 111)
        (srfi 112)
        (srfi 113)
        (except (srfi 114)
                if3 if=? if<? if>? if<=? if>=? if-not=? =? <? >? <=? >=?)
        (srfi 115)
        (srfi 116)
        (srfi 117)
        (srfi 121)
        (except (srfi 123) set!)       ; replaces (srfi 17)
        (srfi 124)
        (except (srfi 125)             ; replaces (srfi 69)
                make-hash-table
                hash-table?
                alist->hash-table
                hash-table-equivalence-function
                hash-table-hash-function
                hash-table-ref
                hash-table-ref/default
                hash-table-set!
                hash-table-delete!
                hash-table-exists?
                hash-table-update!
                hash-table-update!/default
                hash-table-size
                hash-table-keys
                hash-table-values
                hash-table-walk
                hash-table-fold
                hash-table->alist
                hash-table-copy
                hash-table-merge!
                hash
                string-hash
                string-ci-hash
                hash-by-identity
                )
        (srfi 126)
        (srfi 127)
        (except (srfi 128)
                hash-salt string-hash string-ci-hash symbol-hash
                =? <? >? <=? >=?)
        (srfi 129)
        (except (srfi 130)
                string-index string-index-right string-skip string-skip-right)
        (srfi 131)
        (srfi 132)
        (except (srfi 133)             ; replaces (srfi 43 vectors)
                vector-unfold
                vector-unfold-right
                vector-reverse-copy
                vector-concatenate
                vector-empty?
                vector=
                vector-fold
                vector-fold-right
                vector-map!
                vector-count
                vector-index
                vector-index-right
                vector-skip
                vector-skip-right
                vector-binary-search
                vector-any
                vector-every
                vector-swap!
                vector-reverse!
                vector-reverse-copy!
                reverse-vector->list
                reverse-list->vector
                )
        (srfi 134)

        )

(define name-conflicts
  '(
        (except (srfi 5) let)
        (except (srfi 13) string-downcase string-upcase string-titlecase
                          string-for-each string-map string-hash)
        (except (srfi 17) set!)
        (except (srfi 28) format)
        (except (srfi 29) format)
        (except (srfi 43)
                list->vector
                vector-copy vector-copy!
                vector-for-each vector-map
                vector-append vector-fill!)
        (except (srfi 61) cond)
        (except (srfi 63)
                equal? array? make-array array-rank array-ref array-set!)
        (except (srfi 67) string-compare string-compare-ci)
        (except (srfi 69) string-hash string-ci-hash)
        (except (srfi 71) let let* letrec)
        (except (srfi 74) endianness)
        (except (srfi 101)
                cons list make-list pair? null? list? car cdr
                caar cadr cdar cddr
                caaar caadr caddr cadar cdaar cdadr cdddr cddar                
                caaaar caaadr caaddr caadar cadaar cadadr cadddr caddar
                cdaaar cdaadr cdaddr cdadar cddaar cddadr cddddr cdddar
                list-ref list-tail length append reverse
                for-each map quote)
        (except (srfi 114)
                if3 if=? if<? if>? if<=? if>=? if-not=? =? <? >? <=? >=?)
        (except (srfi 123) set!)       ; replaces (srfi 17)
        (except (srfi 125)             ; replaces (srfi 69)
                make-hash-table
                hash-table?
                alist->hash-table
                hash-table-equivalence-function
                hash-table-hash-function
                hash-table-ref
                hash-table-ref/default
                hash-table-set!
                hash-table-delete!
                hash-table-exists?
                hash-table-update!
                hash-table-update!/default
                hash-table-size
                hash-table-keys
                hash-table-values
                hash-table-walk
                hash-table-fold
                hash-table->alist
                hash-table-copy
                hash-table-merge!
                hash
                string-hash
                string-ci-hash
                hash-by-identity
                )
        (except (srfi 128)
                hash-salt string-hash string-ci-hash symbol-hash
                =? <? >? <=? >=?)
        (except (srfi 130)
                string-index string-index-right string-skip string-skip-right)
        (except (srfi 133)             ; replaces (srfi 43 vectors)
                vector-unfold
                vector-unfold-right
                vector-reverse-copy
                vector-concatenate
                vector-empty?
                vector=
                vector-fold
                vector-fold-right
                vector-map!
                vector-count
                vector-index
                vector-index-right
                vector-skip
                vector-skip-right
                vector-binary-search
                vector-any
                vector-every
                vector-swap!
                vector-reverse!
                vector-reverse-copy!
                reverse-vector->list
                reverse-list->vector
                )
        ))

(display "\nAll R7RS and SRFI libraries loaded with these name conflicts:\n\n")

(for-each (lambda (except-clause)
            (write (cadr except-clause))
            (newline)
            (for-each (lambda (name)
                        (display "    ") (write name) (newline))
                      (cddr except-clause)))
          name-conflicts)

(newline)
