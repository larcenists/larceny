;;; SPDX-FileCopyrightText: 2018 John Cowan <cowans32ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 s32)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-s32vector s32vector
          s32vector-unfold s32vector-unfold-right
          s32vector-copy s32vector-reverse-copy 
          s32vector-append s32vector-concatenate
          s32vector-append-subvectors)
  ;; Predicates 
  (export s32? s32vector? s32vector-empty? s32vector=)
  ;; Selectors
  (export s32vector-ref s32vector-length)
  ;; Iteration 
  (export s32vector-take s32vector-take-right
          s32vector-drop s32vector-drop-right
          s32vector-segment
          s32vector-fold s32vector-fold-right
          s32vector-map s32vector-map! s32vector-for-each
          s32vector-count s32vector-cumulate)
  ;; Searching 
  (export s32vector-take-while s32vector-take-while-right
          s32vector-drop-while s32vector-drop-while-right
          s32vector-index s32vector-index-right s32vector-skip s32vector-skip-right 
          s32vector-any s32vector-every s32vector-partition
          s32vector-filter s32vector-remove)
  ;; Mutators 
  (export s32vector-set! s32vector-swap! s32vector-fill! s32vector-reverse!
          s32vector-copy! s32vector-reverse-copy!
          s32vector-unfold! s32vector-unfold-right!)
  ;; Conversion 
  (export s32vector->list list->s32vector
          reverse-s32vector->list reverse-list->s32vector
          s32vector->vector vector->s32vector)
  ;; Misc
  (export make-s32vector-generator s32vector-comparator write-s32vector)

  (include "s32-impl.scm")
)
