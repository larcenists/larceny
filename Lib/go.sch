; Larceny library.
; The `go' procedure -- where uninitialized heaps start.
;
; $Id: go.sch,v 1.3 1997/07/07 20:52:12 lth Exp lth $
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
  (oblist-set! symlist)
  (install-reader)
  (set! scheme-entry #f)
  (main argv))

; eof
