; SRFI 71: Extended LET-syntax for multiple values
;
; $Id$
;
; Conflicts with (rnrs base):
;     extends syntax of let, let*, letrec

(define-library (srfi 71 let)

  (export let let* letrec
          uncons uncons-2 uncons-3 uncons-4 uncons-cons unlist unvector
          values->list values->vector)

  (import (srfi :71 let)))


(define-library (srfi 71)

  (export let let* letrec
          uncons uncons-2 uncons-3 uncons-4 uncons-cons unlist unvector
          values->list values->vector)

  (import (srfi 71 let)))

; eof
