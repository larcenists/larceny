;;; SRFI 47: Array
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs base): equal?
;;;

(define-library (srfi 63 arrays)

  (export array?
          equal?
          array-rank
          array-dimensions
          make-array
          make-shared-array
          list->array
          array->list
          vector->array
          array->vector
          array-in-bounds?
          array-ref
          array-set!
          A:floC128b
          A:floC64b
          A:floC32b
          A:floC16b
          A:floR128b
          A:floR64b
          A:floR32b
          A:floR16b
          A:floR128d
          A:floR64d
          A:floR32d
          A:fixZ64b
          A:fixZ32b
          A:fixZ16b
          A:fixZ8b
          A:fixN64b
          A:fixN32b
          A:fixN16b
          A:fixN8b
          A:bool)

  (import (srfi :63 arrays)))


(define-library (srfi 63)

  (export array?
          equal?
          array-rank
          array-dimensions
          make-array
          make-shared-array
          list->array
          array->list
          vector->array
          array->vector
          array-in-bounds?
          array-ref
          array-set!
          A:floC128b
          A:floC64b
          A:floC32b
          A:floC16b
          A:floR128b
          A:floR64b
          A:floR32b
          A:floR16b
          A:floR128d
          A:floR64d
          A:floR32d
          A:fixZ64b
          A:fixZ32b
          A:fixZ16b
          A:fixZ8b
          A:fixN64b
          A:fixN32b
          A:fixN16b
          A:fixN8b
          A:bool)

  (import (srfi 63 arrays)))

; eof
