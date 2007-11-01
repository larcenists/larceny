; This is a simple R6RS top-level program
; that writes "Hello world!" using R5RS i/o.

(import (rnrs base)
        (rnrs io simple))

(display "Hello world!")
(newline)


