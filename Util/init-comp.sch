; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Code that initializes twobit after loading with load-environment.

(fast-safe-code)                        ; (integrate-usual-procedures #t)
(benchmark-mode #f)                     ; Avoids some surprises
(include-source-code #t)                ; Useful for debugging
(initialize-help "Compiler/" 'brief)

; eof
