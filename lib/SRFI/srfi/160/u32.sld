;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanu32ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 u32)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-u32vector u32vector
          u32vector-unfold u32vector-unfold-right
          u32vector-copy u32vector-reverse-copy 
          u32vector-append u32vector-concatenate
          u32vector-append-subvectors)
  ;; Predicates 
  (export u32? u32vector? u32vector-empty? u32vector=)
  ;; Selectors
  (export u32vector-ref u32vector-length)
  ;; Iteration 
  (export u32vector-take u32vector-take-right
          u32vector-drop u32vector-drop-right
          u32vector-segment
          u32vector-fold u32vector-fold-right
          u32vector-map u32vector-map! u32vector-for-each
          u32vector-count u32vector-cumulate)
  ;; Searching 
  (export u32vector-take-while u32vector-take-while-right
          u32vector-drop-while u32vector-drop-while-right
          u32vector-index u32vector-index-right u32vector-skip u32vector-skip-right 
          u32vector-any u32vector-every u32vector-partition
          u32vector-filter u32vector-remove)
  ;; Mutators 
  (export u32vector-set! u32vector-swap! u32vector-fill! u32vector-reverse!
          u32vector-copy! u32vector-reverse-copy!
          u32vector-unfold! u32vector-unfold-right!)
  ;; Conversion 
  (export u32vector->list list->u32vector
          reverse-u32vector->list reverse-list->u32vector
          u32vector->vector vector->u32vector)
  ;; Misc
  (export make-u32vector-generator u32vector-comparator write-u32vector)

  (include "u32-impl.scm")
)
