(define-library (in-progress hash tables)

  (export

   make-hash-table
   hash-table
   hash-table-tabulate
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
   hash-table-set-entries!
   hash-table-delete!
   hash-table-delete-keys!
   hash-table-extend!
   hash-table-extend!/default
   hash-table-replace!
   hash-table-replace!/default
   hash-table-update!
   hash-table-update!/default
   hash-table-push!
   hash-table-pop!
   hash-table-search!
   hash-table-clear! 

   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count
   hash-table-any
   hash-table-every 

   hash-table-map
   hash-table-map-values
   hash-table-for-each
   hash-table-map!
   hash-table-collect
   hash-table-fold
   hash-table-filter!
   hash-table-remove! 

   hash-table-copy
   hash-table->alist 

   hash-table-accessor
   hash-table-accessor/default 

   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor! 

   hash-table-key-not-found? 
#|
   make-bimap
   bimap?
   bimap-forward-hash-table
   bimap-reverse-hash-table
   bimap-contains?
   bimap-contains-value?
   bimap=?
   bimap-ref
   bimap-value-ref
   bimap-ref/default
   bimap-value-ref/default
   bimap-copy
   bimap-set!
   bimap-set-entries!
   bimap-delete!
   bimap-delete-keys!
   bimap-extend!
   bimap-extend/default!
   bimap-replace!
   bimap-replace/default!
   bimap-update!
   bimap-update/default!
   bimap-clear!
   bimap-filter!
   bimap-remove!
   bimap-partition! 
|#
   )

  (import (scheme base)
          (r6rs hashtables)
          (srfi 114 comparators)
#;        (in-progress hash bimaps))

  (cond-expand
   ((library (scheme char))
    (import (scheme char)))
   (else
    (begin (define string-ci=? string=?))))

  (include "tables.body.scm")

  ) ; eof
