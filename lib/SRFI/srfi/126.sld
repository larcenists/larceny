(define-library (srfi 126)
  (export
   make-eq-hashtable make-eqv-hashtable make-hashtable
   alist->eq-hashtable alist->eqv-hashtable alist->hashtable
   weakness
   hashtable?
   hashtable-size
   hashtable-ref hashtable-set! hashtable-delete!
   hashtable-contains?
   hashtable-lookup hashtable-update! hashtable-intern!
   hashtable-copy hashtable-clear! hashtable-empty-copy
   hashtable-keys hashtable-values hashtable-entries
   hashtable-key-list hashtable-value-list hashtable-entry-lists
   hashtable-walk hashtable-update-all! hashtable-prune! hashtable-merge!
   hashtable-sum hashtable-map->lset hashtable-find
   hashtable-empty? hashtable-pop! hashtable-inc! hashtable-dec!
   hashtable-equivalence-function hashtable-hash-function hashtable-weakness
   hashtable-mutable?
   hash-salt
   (rename rnrs-equal-hash equal-hash)
   (rename rnrs-string-hash string-hash)
   (rename rnrs-string-ci-hash string-ci-hash)
   (rename rnrs-symbol-hash symbol-hash))
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme process-context)
#; (r6rs enums)                      ; commented out for Larceny
   (rnrs enums)                      ; added for Larceny
   (rnrs arithmetic fixnums)         ; added for Larceny
#; (prefix (r6rs hashtables) rnrs-)  ; commented out for Larceny
   (prefix (rnrs hashtables) rnrs-)  ; added for Larceny
   (srfi 1)
   (srfi 27))
  (begin
#;                                   ; commented out for Larceny
    ;; Smallest allowed in R6RS.
    (define (greatest-fixnum) (expt 23 2))

    ;; INCLUDE 126.body.scm

    (include "126.body.scm")         ; added for Larceny
                                     ; (unsure why it was missing)

    ))
