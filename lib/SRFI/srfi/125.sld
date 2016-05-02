(define-library (srfi 125)

  (export

   make-hash-table
   hash-table
;  hash-table-tabulate
   hash-table-unfold
   alist->hash-table 

   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table=?
   hash-table-mutable? 

   hash-table-ref
   hash-table-ref/default 

   hash-table-set!
   hash-table-set-entries!       ; FIXME: listed in index but not specified
   hash-table-delete!
   hash-table-delete-keys!       ; FIXME: listed in index but not specified
   hash-table-intern!
;  hash-table-extend!
;  hash-table-extend!/default
;  hash-table-replace!
;  hash-table-replace!/default
   hash-table-update!
   hash-table-update!/default
   hash-table-push!
   hash-table-pop!
;  hash-table-search!
   hash-table-clear! 

   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count
;  hash-table-any
;  hash-table-every 

   hash-table-map
;  hash-table-map-values
   hash-table-for-each
   hash-table-map!
   hash-table-map->list
;  hash-table-collect
   hash-table-fold
   hash-table-prune!
;  hash-table-filter!
;  hash-table-remove! 

   hash-table-copy
   hash-table-empty-copy
   hash-table->alist 

;  hash-table-accessor
;  hash-table-accessor/default 

   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!

   ;; The following procedures are deprecated by SRFI 125:

   (rename deprecated:hash                     hash)
   (rename deprecated:string-hash              string-hash)
   (rename deprecated:string-ci-hash           string-ci-hash)
   (rename deprecated:hash-by-identity         hash-by-identity)

   (rename deprecated:hash-table-equivalence-function
                                               hash-table-equivalence-function)
   (rename deprecated:hash-table-hash-function hash-table-hash-function)
   (rename deprecated:hash-table-exists?       hash-table-exists?)
   (rename deprecated:hash-table-walk          hash-table-walk)
   (rename deprecated:hash-table-merge!        hash-table-merge!)

   )

  (import (scheme base)
          (scheme write) ; for warnings about deprecated features
          (srfi 126)
          (except (srfi 128)
                  string-hash string-ci-hash symbol-hash hash-salt) ; FIXME
          )

  (cond-expand
   ((library (scheme char))
    (import (scheme char)))
   (else
    (begin (define string-ci=? string=?))))

  (include "125.body.scm")

  ) ; eof