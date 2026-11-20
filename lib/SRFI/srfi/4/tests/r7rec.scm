;; The representation of s8 through s64 vectors

(define-record-type <multivector> (make-multivector tag bv) multivector?
  (tag tag)
  (bv bv))

