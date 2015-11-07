;;; SRFI 43: Vector Library
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs base):
;;;     vector-fill!     (adds two optional arguments)
;;;     vector->list     (adds two optional arguments)
;;;     list->vector     (adds two optional arguments; note bug in SRFI spec)
;;;     vector-map       (allows vectors to be of different lengths,
;;;                       passes the index to the proc)
;;;     vector-for-each  (allows vectors to be of different lengths,
;;;                       passes the index to the proc)
;;;

(define-library (srfi 43 vectors)

  (export

    ;; * Constructors
    make-vector vector
    vector-unfold                   vector-unfold-right
    vector-copy                     vector-reverse-copy
    vector-append                   vector-concatenate
   
    ;; * Predicates
    vector?
    vector-empty?
    vector=
   
    ;; * Selectors
    vector-ref
    vector-length
   
    ;; * Iteration
    vector-fold                     vector-fold-right
    vector-map                      vector-map!
    vector-for-each
    vector-count

    ;; * Searching
    vector-index                    vector-skip
    vector-index-right              vector-skip-right
    vector-binary-search
    vector-any                      vector-every
   
    ;; * Mutators
    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!                    vector-reverse-copy!
   
    ;; * Conversion
    vector->list                    reverse-vector->list
    list->vector                    reverse-list->vector)

  (import (except (srfi :43 vectors) vector->list)
          (only (scheme base) vector->list)))


(define-library (srfi 43)

  (export

    ;; * Constructors
    make-vector vector
    vector-unfold                   vector-unfold-right
    vector-copy                     vector-reverse-copy
    vector-append                   vector-concatenate
   
    ;; * Predicates
    vector?
    vector-empty?
    vector=
   
    ;; * Selectors
    vector-ref
    vector-length
   
    ;; * Iteration
    vector-fold                     vector-fold-right
    vector-map                      vector-map!
    vector-for-each
    vector-count

    ;; * Searching
    vector-index                    vector-skip
    vector-index-right              vector-skip-right
    vector-binary-search
    vector-any                      vector-every
   
    ;; * Mutators
    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!                    vector-reverse-copy!
   
    ;; * Conversion
    vector->list                    reverse-vector->list
    list->vector                    reverse-list->vector)

  (import (srfi 43 vectors)))

; eof
