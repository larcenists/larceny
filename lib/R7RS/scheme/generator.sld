;;; (scheme generator)
;;;
;;; R7RS Red Edition

(define-library (scheme generator)
  (export generator make-iota-generator make-range-generator 
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator
          gcons* gappend gcombine gfilter gremove 
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect
          generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold
          )
  (import (srfi 121)))
