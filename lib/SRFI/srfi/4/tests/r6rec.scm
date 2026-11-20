;; The representation of s8 through s64 vectors

(define-record-type (<multivector> make-multivector multivector?)
  (fields (immutable tag tag)
          (immutable bv bv)))

