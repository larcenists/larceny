; Larceny library.
; The `go' procedure -- where uninitialized heaps start.
;
; $Id: go.sch,v 1.2 1997/02/03 20:07:13 lth Exp $
;
; Initializes the system and calls "main". This procedure is only called
; when we are loading an unitialized heap; initialized heaps will have "main"
; (or something else) as their entry point.
;
; When the system tables have been initialized, this procedure attempts to
; exterminate itself by clobbering 'scheme-entry' and calling 'main' in a
; tail-recursive fashion, thereby making itself and the caller eligible for
; garbage collection if they have not been referenced from anywhere else (and
; they should not have been).

(define (go symlist argv)
  (install-millicode-support)
;  (install-memstats-vector)
  (install-symbols symlist 521)   ; 521 is almost completely arbitrary
  (install-reader)
  (set! scheme-entry #f)
  (main argv))

; eof
