;;; (scheme sort)
;;;
;;; R7RS Red Edition

(define-library (scheme sort)

  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!
          )

  (import (srfi 132 sorting)))
