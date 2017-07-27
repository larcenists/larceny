;;; SRFI 122: Nonempty Intervals and Generalized Arrays

(define-library (srfi 122)

  (export

   translation?
   permutation?

   
   make-interval
   interval?
   interval-dimension
   interval-lower-bound
   interval-upper-bound
   interval-lower-bounds->list
   interval-upper-bounds->list
   interval-lower-bounds->vector
   interval-upper-bounds->vector
   interval=
   interval-volume
   interval-subset?
   interval-contains-multi-index?
   interval-projections
   interval-for-each
   interval-dilate
   interval-intersect
   interval-translate
   interval-permute
   interval-scale

   make-storage-class
   storage-class?
   storage-class-getter
   storage-class-setter
   storage-class-checker
   storage-class-maker
   storage-class-length
   storage-class-default
   generic-storage-class
   s8-storage-class
   s16-storage-class
   s32-storage-class
   s64-storage-class
   u1-storage-class
   u8-storage-class
   u16-storage-class
   u32-storage-class
   u64-storage-class
   f32-storage-class
   f64-storage-class
   c64-storage-class
   c128-storage-class

   make-array
   array?
   array-domain
   array-getter
   array-dimension
   mutable-array?
   array-setter
   specialized-array-default-safe?
   make-specialized-array
   specialized-array?
   array-storage-class
   array-indexer
   array-body
   array-safe?
   specialized-array-share
   array->specialized-array
   array-curry
   array-extract
   array-translate
   array-permute
   array-reverse
   array-sample
   array-map
   array-for-each
   array-fold
   array-fold-right
   array-any
   array-every
   array->list
   list->specialized-array

   )

  (import (rename (scheme base)
                  (error r7rs:error))
          (scheme cxr)
          (scheme complex)
          (scheme case-lambda)
          (only (scheme list)
                every iota)
          (rename (only (scheme list)
                        remove take)
                  (remove list-remove)
                  (take list-take))
          (only (scheme vector)
                vector-every)
          (srfi 143)                ; fixnums
          (only (srfi 144) flonum?) ; flonums
          (except (rnrs bytevectors)
                  bytevector-copy!)
          )

  (import (scheme write)) ; FIXME

  (include "122.body.scm"))
