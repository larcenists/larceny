; REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
; ==========================================
;
; $Id$
;

(define-library (srfi 26 cut)

  (export cut cute)

  (import (srfi :26 cut)))


(define-library (srfi 26)

  (export cut cute <> <...>)

  (import (scheme base) (srfi 26 cut))


  (begin

(define-syntax <>
  (syntax-rules ()
    ((<> . args)
     (syntax-error "invalid use of auxiliary syntax" (<> . args)))))

(define-syntax <...>
  (syntax-rules ()
    ((<...> . args)
     (syntax-error "invalid use of auxiliary syntax" (<...> . args)))))

))

; eof
