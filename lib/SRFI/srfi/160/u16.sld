;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanu16ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 u16)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-u16vector u16vector
          u16vector-unfold u16vector-unfold-right
          u16vector-copy u16vector-reverse-copy 
          u16vector-append u16vector-concatenate
          u16vector-append-subvectors)
  ;; Predicates 
  (export u16? u16vector? u16vector-empty? u16vector=)
  ;; Selectors
  (export u16vector-ref u16vector-length)
  ;; Iteration 
  (export u16vector-take u16vector-take-right
          u16vector-drop u16vector-drop-right
          u16vector-segment
          u16vector-fold u16vector-fold-right
          u16vector-map u16vector-map! u16vector-for-each
          u16vector-count u16vector-cumulate)
  ;; Searching 
  (export u16vector-take-while u16vector-take-while-right
          u16vector-drop-while u16vector-drop-while-right
          u16vector-index u16vector-index-right u16vector-skip u16vector-skip-right 
          u16vector-any u16vector-every u16vector-partition
          u16vector-filter u16vector-remove)
  ;; Mutators 
  (export u16vector-set! u16vector-swap! u16vector-fill! u16vector-reverse!
          u16vector-copy! u16vector-reverse-copy!
          u16vector-unfold! u16vector-unfold-right!)
  ;; Conversion 
  (export u16vector->list list->u16vector
          reverse-u16vector->list reverse-list->u16vector
          u16vector->vector vector->u16vector)
  ;; Misc
  (export make-u16vector-generator u16vector-comparator write-u16vector)

  (include "u16-impl.scm")
)
