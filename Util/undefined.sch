; Copyright 1999 Lars T Hansen
;
; $Id$
;
; List all variables that have #!undefined value in the environment.

(define (undefined-vars env)
  (filter (lambda (v)
            (not (environment-gettable? env v)))
          (environment-variables env)))

; eof
