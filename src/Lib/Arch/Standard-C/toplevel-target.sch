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

  (environment-set! larc 'sys$c-ffi-apply sys$c-ffi-apply)
  (environment-set! larc 'sys$c-ffi-dlopen sys$c-ffi-dlopen)
  (environment-set! larc 'sys$c-ffi-dlsym sys$c-ffi-dlsym)
  (environment-set! larc 'peek-bytes peek-bytes)
  (environment-set! larc 'poke-bytes poke-bytes)

  ;; environment interface

  (environment-set! larc 'dump-heap dump-heap)
  (environment-set! larc 'dump-interactive-heap dump-interactive-heap)

  ;; Support for loading compiled files as code-less FASL files with
  ;; the code vectors already linked into the executable or present
  ;; in dynamically loaded object files.

  (environment-set! larc '.petit-patch-procedure .petit-patch-procedure)
  (environment-set! larc '.petit-shared-object .petit-shared-object)

  larc)

; eof
