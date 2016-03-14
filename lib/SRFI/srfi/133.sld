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
  (import (except (scheme base)
                  vector-copy vector-copy!
                  vector->list list->vector
                  vector->string string->vector
                  vector-append vector-fill!
                  vector-map vector-for-each)
          (scheme cxr))

  (include "133.body.scm")
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
