;;; SRFI-1: list-processing library 			-*- Scheme -*-
;;; Reference implementation
;;;
;;; $Id$
;;;
;;; Conflicts with (rnrs base):
;;;     map, for-each: allow list arguments of different lengths
;;; Conflicts with (rnrs lists):
;;;     member, assoc: accept an optional third argument
;;;     fold-right: different arguments, different semantics
;;;

(define-library (srfi 1 lists)

  (export cons list
          xcons cons* make-list list-tabulate
          list-copy circular-list iota

          pair? null?
          proper-list? circular-list? dotted-list? 
          not-pair? null-list?
          list=

          car cdr
          caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          list-ref
          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr
          take       drop
          take-right drop-right
          take!      drop-right! 
          split-at   split-at! 
          last last-pair

          length length+
          append  concatenate  reverse
          append! concatenate! reverse!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count

          map for-each
          fold       unfold       pair-fold       reduce 
          fold-right unfold-right pair-fold-right reduce-right 
          append-map append-map!
          map! pair-for-each filter-map map-in-order

          filter  partition  remove
          filter! partition! remove!

          member memq memv
          find find-tail 
          any every
          list-index
          take-while drop-while take-while!
          span break span! break!

          delete  delete-duplicates 
          delete! delete-duplicates!

          assoc assq assv
          alist-cons alist-copy
          alist-delete alist-delete!

          lset<= lset=           lset-adjoin
          lset-union             lset-union!
          lset-intersection      lset-intersection!
          lset-difference        lset-difference!
          lset-xor               lset-xor!
          lset-diff+intersection lset-diff+intersection!

          set-car! set-cdr!)

  (import (srfi :1 lists)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (srfi 1)

  (export cons list
          xcons cons* make-list list-tabulate
          list-copy circular-list iota

          pair? null?
          proper-list? circular-list? dotted-list? 
          not-pair? null-list?
          list=

          car cdr
          caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          list-ref
          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr
          take       drop
          take-right drop-right
          take!      drop-right! 
          split-at   split-at! 
          last last-pair

          length length+
          append  concatenate  reverse
          append! concatenate! reverse!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count

          map for-each
          fold       unfold       pair-fold       reduce 
          fold-right unfold-right pair-fold-right reduce-right 
          append-map append-map!
          map! pair-for-each filter-map map-in-order

          filter  partition  remove
          filter! partition! remove!

          member memq memv
          find find-tail 
          any every
          list-index
          take-while drop-while take-while!
          span break span! break!

          delete  delete-duplicates 
          delete! delete-duplicates!

          assoc assq assv
          alist-cons alist-copy
          alist-delete alist-delete!

          lset<= lset=           lset-adjoin
          lset-union             lset-union!
          lset-intersection      lset-intersection!
          lset-difference        lset-difference!
          lset-xor               lset-xor!
          lset-diff+intersection lset-diff+intersection!

          set-car! set-cdr!)

  (import (srfi 1 lists)))

; eof
