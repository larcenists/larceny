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

  (environment-set! larc 'sys$C-ffi-apply sys$C-ffi-apply)
  (environment-set! larc 'sys$C-ffi-dlopen sys$C-ffi-dlopen)
  (environment-set! larc 'sys$C-ffi-dlsym sys$C-ffi-dlsym)
  (environment-set! larc 'peek-bytes peek-bytes)
  (environment-set! larc 'poke-bytes poke-bytes)

  ;; Support for loading compiled files as code-less FASL files with
  ;; the code vectors already linked into the executable or present
  ;; in dynamically loaded object files.

  (environment-set! larc '.petit-patch-procedure .petit-patch-procedure)
  (environment-set! larc '.petit-shared-object .petit-shared-object)

  larc)

; eof
