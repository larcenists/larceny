; The `go' procedure.
;
; Initializes the system and calls "main". This procedure is only called
; when we are loading an unitialized heap; initialized heaps will have "main"
; (or something else) as their entry point.
;
; $Id$

(define (go symlist)
  (install-millicode-support)
  (install-symbols symlist 521)   ; 521 is completely arbitrary
  (install-reader)
  (main)
  ; really need to flush all output ports here (or "exit" should do that.)
  (exit))


