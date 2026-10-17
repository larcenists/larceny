;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanu64ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 u64)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-u64vector u64vector
          u64vector-unfold u64vector-unfold-right
          u64vector-copy u64vector-reverse-copy 
          u64vector-append u64vector-concatenate
          u64vector-append-subvectors)
  ;; Predicates 
  (export u64? u64vector? u64vector-empty? u64vector=)
  ;; Selectors
  (export u64vector-ref u64vector-length)
  ;; Iteration 
  (export u64vector-take u64vector-take-right
          u64vector-drop u64vector-drop-right
          u64vector-segment
          u64vector-fold u64vector-fold-right
          u64vector-map u64vector-map! u64vector-for-each
          u64vector-count u64vector-cumulate)
  ;; Searching 
  (export u64vector-take-while u64vector-take-while-right
          u64vector-drop-while u64vector-drop-while-right
          u64vector-index u64vector-index-right u64vector-skip u64vector-skip-right 
          u64vector-any u64vector-every u64vector-partition
          u64vector-filter u64vector-remove)
  ;; Mutators 
  (export u64vector-set! u64vector-swap! u64vector-fill! u64vector-reverse!
          u64vector-copy! u64vector-reverse-copy!
          u64vector-unfold! u64vector-unfold-right!)
  ;; Conversion 
  (export u64vector->list list->u64vector
          reverse-u64vector->list reverse-list->u64vector
          u64vector->vector vector->u64vector)
  ;; Misc
  (export make-u64vector-generator u64vector-comparator write-u64vector)

  (include "u64-impl.scm")
)
