; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Macro expander interface for the interpreter and the top-level
; environment.

($$trace "macro-expand")

; Compiler switches used by Twobit's macro expander, redefined here
; for when the expander is used by the interpreter.

(define (issue-warnings) #t)
(define (include-source-code) #t)
(define (include-procedure-names) #t)
(define (include-variable-names) #t)
(define (integrate-usual-procedures) #f)
(define (benchmark-mode) #f)

; Used by the interpreter -- this will eventually change so that
; the interpreter core can use the procedure documentation structure.

(define (interpreter-macro-expand expr . rest)
  (let ((env (if (null? rest)
		 (interaction-environment)
		 (car rest))))
    ; ENV is currently unused.
    (make-readable
     (macro-expand expr))))

; Exported to the user environment.

(define (toplevel-macro-expand expr . rest)
  (let ((env (if (null? rest)
		 (interaction-environment)
		 (car rest))))
    ; ENV is currently unused.
    (make-readable
     (macro-expand expr))))

; `Twobit-sort' is used by Twobit's macro expander.

(define (twobit-sort less? list)
  (sort list less?))
  
; eof

