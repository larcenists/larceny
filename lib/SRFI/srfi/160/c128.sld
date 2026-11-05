;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanc128ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 c128)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-c128vector c128vector
          c128vector-unfold c128vector-unfold-right
          c128vector-copy c128vector-reverse-copy 
          c128vector-append c128vector-concatenate
          c128vector-append-subvectors)
  ;; Predicates 
  (export c128? c128vector? c128vector-empty? c128vector=)
  ;; Selectors
  (export c128vector-ref c128vector-length)
  ;; Iteration 
  (export c128vector-take c128vector-take-right
          c128vector-drop c128vector-drop-right
          c128vector-segment
          c128vector-fold c128vector-fold-right
          c128vector-map c128vector-map! c128vector-for-each
          c128vector-count c128vector-cumulate)
  ;; Searching 
  (export c128vector-take-while c128vector-take-while-right
          c128vector-drop-while c128vector-drop-while-right
          c128vector-index c128vector-index-right c128vector-skip c128vector-skip-right 
          c128vector-any c128vector-every c128vector-partition
          c128vector-filter c128vector-remove)
  ;; Mutators 
  (export c128vector-set! c128vector-swap! c128vector-fill! c128vector-reverse!
          c128vector-copy! c128vector-reverse-copy!
          c128vector-unfold! c128vector-unfold-right!)
  ;; Conversion 
  (export c128vector->list list->c128vector
          reverse-c128vector->list reverse-list->c128vector
          c128vector->vector vector->c128vector)
  ;; Misc
  (export make-c128vector-generator c128vector-comparator write-c128vector)

  (include "c128-impl.scm")
)
