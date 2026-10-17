;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanf32ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 f32)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-f32vector f32vector
          f32vector-unfold f32vector-unfold-right
          f32vector-copy f32vector-reverse-copy 
          f32vector-append f32vector-concatenate
          f32vector-append-subvectors)
  ;; Predicates 
  (export f32? f32vector? f32vector-empty? f32vector=)
  ;; Selectors
  (export f32vector-ref f32vector-length)
  ;; Iteration 
  (export f32vector-take f32vector-take-right
          f32vector-drop f32vector-drop-right
          f32vector-segment
          f32vector-fold f32vector-fold-right
          f32vector-map f32vector-map! f32vector-for-each
          f32vector-count f32vector-cumulate)
  ;; Searching 
  (export f32vector-take-while f32vector-take-while-right
          f32vector-drop-while f32vector-drop-while-right
          f32vector-index f32vector-index-right f32vector-skip f32vector-skip-right 
          f32vector-any f32vector-every f32vector-partition
          f32vector-filter f32vector-remove)
  ;; Mutators 
  (export f32vector-set! f32vector-swap! f32vector-fill! f32vector-reverse!
          f32vector-copy! f32vector-reverse-copy!
          f32vector-unfold! f32vector-unfold-right!)
  ;; Conversion 
  (export f32vector->list list->f32vector
          reverse-f32vector->list reverse-list->f32vector
          f32vector->vector vector->f32vector)
  ;; Misc
  (export make-f32vector-generator f32vector-comparator write-f32vector)

  (include "f32-impl.scm")
)
