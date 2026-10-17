;;; SPDX-FileCopyrightText: 2018 John Cowan <cowans16ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 s16)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-s16vector s16vector
          s16vector-unfold s16vector-unfold-right
          s16vector-copy s16vector-reverse-copy 
          s16vector-append s16vector-concatenate
          s16vector-append-subvectors)
  ;; Predicates 
  (export s16? s16vector? s16vector-empty? s16vector=)
  ;; Selectors
  (export s16vector-ref s16vector-length)
  ;; Iteration 
  (export s16vector-take s16vector-take-right
          s16vector-drop s16vector-drop-right
          s16vector-segment
          s16vector-fold s16vector-fold-right
          s16vector-map s16vector-map! s16vector-for-each
          s16vector-count s16vector-cumulate)
  ;; Searching 
  (export s16vector-take-while s16vector-take-while-right
          s16vector-drop-while s16vector-drop-while-right
          s16vector-index s16vector-index-right s16vector-skip s16vector-skip-right 
          s16vector-any s16vector-every s16vector-partition
          s16vector-filter s16vector-remove)
  ;; Mutators 
  (export s16vector-set! s16vector-swap! s16vector-fill! s16vector-reverse!
          s16vector-copy! s16vector-reverse-copy!
          s16vector-unfold! s16vector-unfold-right!)
  ;; Conversion 
  (export s16vector->list list->s16vector
          reverse-s16vector->list reverse-list->s16vector
          s16vector->vector vector->s16vector)
  ;; Misc
  (export make-s16vector-generator s16vector-comparator write-s16vector)

  (include "s16-impl.scm")
)
