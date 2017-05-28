(define-library (srfi 133 vectors)
  (export ;; Constructors 
          vector-unfold vector-unfold-right vector-reverse-copy 
          vector-concatenate vector-append-subvectors
          ;; Predicates 
          vector-empty? vector=
          ;; Iteration 
          vector-fold vector-fold-right vector-map!
          vector-count vector-cumulate
          ;; Searching 
          vector-index vector-index-right vector-skip vector-skip-right 
          vector-binary-search vector-any vector-every vector-partition
          ;; Mutators 
          vector-swap! vector-reverse! 
          vector-reverse-copy! vector-unfold! vector-unfold-right!
          ;; Conversion 
          reverse-vector->list reverse-list->vector
          )
  (import (scheme base)
          (scheme cxr))

  (include "133.body.scm")

  ;; Using vector-map to implement vector-map! takes more space,
  ;; but the simplicity and reliability of maintaining just one
  ;; version wins out.  When there's more than one vector, this
  ;; will be faster as well.

  (begin
   (define (vector-map! f vec . vectors)
     (vector-copy! vec 0 (apply vector-map f vec vectors))))
  )


(define-library (srfi 133)
  (export ;; Constructors 
          vector-unfold vector-unfold-right vector-reverse-copy 
          vector-concatenate vector-append-subvectors
          ;; Predicates 
          vector-empty? vector=
          ;; Iteration 
          vector-fold vector-fold-right vector-map!
          vector-count vector-cumulate
          ;; Searching 
          vector-index vector-index-right vector-skip vector-skip-right 
          vector-binary-search vector-any vector-every vector-partition
          ;; Mutators 
          vector-swap! vector-reverse! 
          vector-reverse-copy! vector-unfold! vector-unfold-right!
          ;; Conversion 
          reverse-vector->list reverse-list->vector
          )
  (import (srfi 133 vectors))
  )
