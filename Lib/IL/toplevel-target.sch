; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; The interpreter's top-level environment -- Standard-C additions

($$trace "toplevel-standard-c")

(define (initialize-null-environment-target-specific null) null)
(define (initialize-r4rs-environment-target-specific r4rs) r4rs)
(define (initialize-r5rs-environment-target-specific r5rs) r5rs)

(define (initialize-larceny-environment-target-specific larc) 

  ;; system performance and interface

  ;; Support for loading compiled files as code-less FASL files with
  ;; the code vectors already linked into the executable or present
  ;; in dynamically loaded object files.

  (environment-set! larc '.common-patch-procedure .common-patch-procedure)

  ;; Load extensions
  (for-each (lambda (p) (p larc))
            *larceny-environment-extensions*)
  (set! *larceny-environment-extensions* (undefined))
  
  larc)

;; *larceny-environment-extensions* : (listof (environment -> void))
;; A list of procedures which accept an environment. The procedures are 
;; expected to extend the environment with new bindings.
(define *larceny-environment-extensions* '())

;; *interactive-eval-list* : (listof s-expr)
;; A list of forms to be evaluated when the interpreter starts.
(define *interactive-eval-list* '())


; eof
