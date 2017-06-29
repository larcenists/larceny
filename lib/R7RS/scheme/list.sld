;;; (scheme list)                   			-*- Scheme -*-
;;;
;;; R7RS Red Edition
;;; SRFI-1: list-processing library
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs base):
;;;     map, for-each: allow list arguments of different lengths
;;; Conflicts with (rnrs lists):
;;;     member, assoc: accept an optional third argument
;;;     fold-right: different arguments, different semantics
;;;

;;; Red Edition libraries aren't supposed to export any identifiers
;;; that are defined by any library defined by R7RS small:
;;; https://groups.google.com/forum/#!topic/scheme-reports-wg2/NZA9CIMRl48

(define-library (scheme list)

  (export
          cons                ; part of R7RS small
          list                ; part of R7RS small
          xcons
          cons*
          make-list           ; part of R7RS small
          list-tabulate
          list-copy           ; part of R7RS small
          circular-list
          iota

          pair?               ; part of R7RS small
          null?               ; part of R7RS small
          proper-list?
          circular-list?
          dotted-list? 
          not-pair?
          null-list?
          list=

          car                 ; part of R7RS small
          cdr                 ; part of R7RS small
          caar                ; part of R7RS small
          cadr                ; part of R7RS small
          cdar                ; part of R7RS small
          cddr                ; part of R7RS small
          caaar               ; part of R7RS small
          caadr               ; part of R7RS small
          cadar               ; part of R7RS small
          caddr               ; part of R7RS small
          cdaar               ; part of R7RS small
          cdadr               ; part of R7RS small
          cddar               ; part of R7RS small
          cdddr               ; part of R7RS small
          caaaar              ; part of R7RS small
          caaadr              ; part of R7RS small
          caadar              ; part of R7RS small
          caaddr              ; part of R7RS small
          cadaar              ; part of R7RS small
          cadadr              ; part of R7RS small
          caddar              ; part of R7RS small
          cadddr              ; part of R7RS small
          cdaaar              ; part of R7RS small
          cdaadr              ; part of R7RS small
          cdadar              ; part of R7RS small
          cdaddr              ; part of R7RS small
          cddaar              ; part of R7RS small
          cddadr              ; part of R7RS small
          cdddar              ; part of R7RS small
          cddddr              ; part of R7RS small
          list-ref            ; part of R7RS small
          first
          second
          third
          fourth
          fifth
          sixth
          seventh
          eighth
          ninth
          tenth
          car+cdr
          take
          drop
          take-right
          drop-right
          take!
          drop-right! 
          split-at
          split-at! 
          last
          last-pair

          length              ; part of R7RS small
          length+
          append              ; part of R7RS small
          concatenate
          reverse             ; part of R7RS small
          append!
          concatenate!
          reverse!
          append-reverse
          append-reverse!
          zip
          unzip1
          unzip2
          unzip3
          unzip4
          unzip5
          count

          map                 ; part of R7RS small
          for-each            ; part of R7RS small
          fold
          unfold
          pair-fold
          reduce 
          fold-right
          unfold-right
          pair-fold-right
          reduce-right 
          append-map
          append-map!
          map!
          pair-for-each
          filter-map
          map-in-order

          filter
          partition
          remove
          filter!
          partition!
          remove!

          member              ; part of R7RS small
          memq                ; part of R7RS small
          memv                ; part of R7RS small
          find
          find-tail 
          any
          every
          list-index
          take-while
          drop-while
          take-while!
          span
          break
          span!
          break!

          delete
          delete-duplicates 
          delete!
          delete-duplicates!

          assoc               ; part of R7RS small
          assq                ; part of R7RS small
          assv                ; part of R7RS small
          alist-cons
          alist-copy
          alist-delete
          alist-delete!

          lset<=
          lset=
          lset-adjoin
          lset-union
          lset-union!
          lset-intersection
          lset-intersection!
          lset-difference
          lset-difference!
          lset-xor
          lset-xor!
          lset-diff+intersection
          lset-diff+intersection!

          set-car!            ; part of R7RS small
          set-cdr!            ; part of R7RS small
          )

  (import (srfi 1 lists)))
