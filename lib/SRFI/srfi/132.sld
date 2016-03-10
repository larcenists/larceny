;;; Olin's test harness tests some procedures that aren't part of SRFI 132,
;;; so (srfi 132 plus) is here just to support Olin's tests.

(define-library (srfi 132 plus)

  (export list-sorted? vector-sorted?
          list-merge-sort  vector-merge-sort               ; not part of SRFI 132
          list-merge-sort! vector-merge-sort!              ; not part of SRFI 132
          vector-insert-sort vector-insert-sort!           ; not part of SRFI 132
          vector-heap-sort   vector-heap-sort!             ; not part of SRFI 132
          vector-quick-sort  vector-quick-sort!            ; not part of SRFI 132
;         vector-binary-search vector-binary-search3       ; not part of SRFI 132
          vector-quick-sort3 vector-quick-sort3!           ; not part of SRFI 132
          list-merge vector-merge
          list-sort vector-sort
          list-stable-sort vector-stable-sort
          list-merge! vector-merge!
          list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!)

  (import (except (scheme base) vector-copy)
          (scheme cxr))

  (include "132/delndups.scm")
  (include "132/lmsort.scm")
  (include "132/sortp.scm")
  (include "132/vector-util.scm")
  (include "132/vhsort.scm")
  (include "132/visort.scm")
  (include "132/vmsort.scm")
  (include "132/vqsort2.scm")
  (include "132/vqsort3.scm")
  (include "132/sort.scm") ; must be last
  )

(define-library (srfi 132)

  (export list-sorted? vector-sorted?
          list-merge vector-merge
          list-sort vector-sort
          list-stable-sort vector-stable-sort
          list-merge! vector-merge!
          list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!)

  (import (srfi 132 plus)))
