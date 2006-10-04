; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; The interpreter's top-level environment -- SPARC additions

($$trace "toplevel-standard-c")

(define (initialize-null-environment-target-specific null) null)
(define (initialize-r4rs-environment-target-specific r4rs) r4rs)
(define (initialize-r5rs-environment-target-specific r5rs) r5rs)

(define (initialize-larceny-environment-target-specific larc) 

  ;; numbers

  (environment-set! larc 'fx+ fx+)
  (environment-set! larc 'fx- fx-)
  (environment-set! larc 'fx-- fx--)
  (environment-set! larc 'fx* fx*)
  (environment-set! larc 'fxzero? fxzero?)
  (environment-set! larc 'fxpositive? fxpositive?)
  (environment-set! larc 'fxnegative? fxnegative?)
  (environment-set! larc 'fx= fx=)
  (environment-set! larc 'fx< fx<)
  (environment-set! larc 'fx<= fx<=)
  (environment-set! larc 'fx> fx>)
  (environment-set! larc 'fx>= fx>=)
  (environment-set! larc 'fl+ fl+)
  (environment-set! larc 'fl- fl-)
  (environment-set! larc 'fl-- fl--)
  (environment-set! larc 'fl* fl*)
  (environment-set! larc 'fl= fl=)
  (environment-set! larc 'fl< fl<)
  (environment-set! larc 'fl<= fl<=)
  (environment-set! larc 'fl> fl>)
  (environment-set! larc 'fl>= fl>=)

  ;; system performance and interface

  (environment-set! larc 'sys$c-ffi-apply sys$c-ffi-apply)
  (environment-set! larc 'sys$c-ffi-dlopen sys$c-ffi-dlopen)
  (environment-set! larc 'sys$c-ffi-dlsym sys$c-ffi-dlsym)
  (environment-set! larc 'peek-bytes peek-bytes)
  (environment-set! larc 'poke-bytes poke-bytes)

  ;; environment interface

  (environment-set! larc 'dump-heap dump-heap)
  (environment-set! larc 'dump-interactive-heap dump-interactive-heap)

  ;; miscellaneous extensions and hacks

  (environment-set! larc 'sys$codevector-iflush sys$codevector-iflush)

  larc)

; eof
