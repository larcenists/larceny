; Copyright 1998 Lars T Hansen
;
; $Id$
;
; A collection of useful and common macros.
;
; Some macros that used to be here are now in src/Compiler/usual.sch :
;
;     when
;     unless
;     case-lambda
;
; Some macros that used to be here are no longer part of the standard
; heaps, but are still available via the require mechanism:
;
; lib/Standard/common-syntax.sch :
;
;     while
;     repeat
;
; lib/Standard/fluid.sch :
;
;     fluid-let

(define-syntax bound?
  (syntax-rules ()
    ((bound? x)
     (bound? x (interaction-environment)))
    ((bound? ?x ?env)
     (let ((env ?env)
           (name (quote ?x)))
       (or (environment-variable? env name)
           (environment-macro? env name))))))

(define-syntax time
  (syntax-rules ()
    ((time ?expr)
     (run-with-stats (lambda () ?expr)))))

; eof
