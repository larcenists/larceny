;;; This is SRFI 4, extended with c64 and c128 vectors.
;;; It also serves to isolate Larceny-specific dependencies,
;;; which are exported for use by SRFI 160.

(define-library (srfi 4 extended)
  (import (scheme base))
  (import (rnrs bytevectors))
  (import (primitives make-numvec new-numvec numeric-vector?
                      larceny:numvec.hashcode larceny:numvec.bv
                      larceny:numvec-getter larceny:numvec-setter
                      larceny:numvec-predicate
		      larceny:numvec->list
                      bytevector-c64-native-ref bytevector-c64-native-set!
                      bytevector-c128-native-ref bytevector-c128-native-set!
                      u8? s8? u16? s16? u32? s32? u64? s64?
                      f32? f64? c64? c128?

                      larceny:numvec-hashcode->element-width
                      larceny:numvec-hashcode->int-tag
                      larceny:numvec-hashcode->symbol-tag
                      larceny:numvec-symtag->int-tag
                      larceny:numvec-symtag->width

                      ;; These will not be exported

		      list->bytevector fxlogand fxlsh
		      random))

  (export
   make-numvec new-numvec numeric-vector?
   larceny:numvec.hashcode larceny:numvec.bv
   larceny:numvec-getter larceny:numvec-setter
   larceny:numvec-predicate
   bytevector-c64-native-ref bytevector-c64-native-set!
   bytevector-c128-native-ref bytevector-c128-native-set!
   u8? s8? u16? s16? u32? s32? u64? s64?
   f32? f64? c64? c128?

   larceny:numvec-hashcode->element-width
   larceny:numvec-hashcode->int-tag
   larceny:numvec-hashcode->symbol-tag
   larceny:numvec-symtag->int-tag
   larceny:numvec-symtag->width)

  (export
   make-u8vector make-s8vector make-u16vector make-s16vector make-u32vector
   make-s32vector make-u64vector make-s64vector make-f32vector make-f64vector
   make-c64vector make-c128vector
   u8vector s8vector u16vector s16vector u32vector
   s32vector u64vector s64vector f32vector f64vector
   c64vector c128vector
   u8vector?  s8vector?  u16vector?  s16vector?  u32vector?
   s32vector?  u64vector?  s64vector?  f32vector?  f64vector?
   c64vector? c128vector?
   u8vector-length s8vector-length u16vector-length s16vector-length
   u32vector-length s32vector-length u64vector-length s64vector-length
   f32vector-length f64vector-length c64vector-length c128vector-length
   u8vector-ref s8vector-ref u16vector-ref s16vector-ref u32vector-ref
   s32vector-ref u64vector-ref s64vector-ref f32vector-ref f64vector-ref
   c64vector-ref c128vector-ref
   u8vector-set!  s8vector-set!  u16vector-set!  s16vector-set!
   u32vector-set! s32vector-set! u64vector-set!  s64vector-set!
   f32vector-set!  f64vector-set! c64vector-set! c128vector-set!
   u8vector->list s8vector->list u16vector->list s16vector->list
   u32vector->list s32vector->list u64vector->list s64vector->list
   f32vector->list f64vector->list  c64vector->list c128vector->list
   list->u8vector list->s8vector list->u16vector list->s16vector
   list->u32vector list->s32vector list->u64vector list->s64vector
   list->f32vector list->f64vector  list->c64vector list->c128vector)

  (include "impl.scm")
  )
