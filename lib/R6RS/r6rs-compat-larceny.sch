;;;===============================================================================
;;;
;;; Larceny compatibility file
;;;
;;; Uncomment appropriate LOAD comand in macros-core.scm
;;;
;;;===============================================================================

; $Id$

;; A numeric string that uniquely identifies this run in the universe.

(define (ex:unique-token)
  (number->string (current-seconds) 16))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "\x0;")
(define ex:free-prefix "\x1;")
