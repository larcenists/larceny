(define-library (in-progress hash bimaps)

  (export

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

   )

  (import (scheme base)
          (r6rs hashtables)
          (in-progress hash tables))

  (include "bimaps.body.scm")

  ) ; eof
