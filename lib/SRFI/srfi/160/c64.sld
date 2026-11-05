;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanc64ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 160 c64)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-c64vector c64vector
          c64vector-unfold c64vector-unfold-right
          c64vector-copy c64vector-reverse-copy 
          c64vector-append c64vector-concatenate
          c64vector-append-subvectors)
  ;; Predicates 
  (export c64? c64vector? c64vector-empty? c64vector=)
  ;; Selectors
  (export c64vector-ref c64vector-length)
  ;; Iteration 
  (export c64vector-take c64vector-take-right
          c64vector-drop c64vector-drop-right
          c64vector-segment
          c64vector-fold c64vector-fold-right
          c64vector-map c64vector-map! c64vector-for-each
          c64vector-count c64vector-cumulate)
  ;; Searching 
  (export c64vector-take-while c64vector-take-while-right
          c64vector-drop-while c64vector-drop-while-right
          c64vector-index c64vector-index-right c64vector-skip c64vector-skip-right 
          c64vector-any c64vector-every c64vector-partition
          c64vector-filter c64vector-remove)
  ;; Mutators 
  (export c64vector-set! c64vector-swap! c64vector-fill! c64vector-reverse!
          c64vector-copy! c64vector-reverse-copy!
          c64vector-unfold! c64vector-unfold-right!)
  ;; Conversion 
  (export c64vector->list list->c64vector
          reverse-c64vector->list reverse-list->c64vector
          c64vector->vector vector->c64vector)
  ;; Misc
  (export make-c64vector-generator c64vector-comparator write-c64vector)

  (include "c64-impl.scm")
)
