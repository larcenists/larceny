; Environments, as adopted for R5RS.
; $Id: env.sch,v 1.1 1997/02/03 20:07:13 lth Exp $
;
; Lars Thomas Hansen / March 26, 1996
;
; For now, just a placeholder utilizing the existing toplevel-environment
; code; these procedures do not really correspond to the specifications
; outlined in the proposal (Lisp Pointers V.4 (Oct-Dec 1992).
;
; In Larceny, these will be built on top of real first-class toplevel
; environments (namespaces).

($$trace "env")

(define *env-tag* (list 'environment))

(define (environment? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) *env-tag*)))

(define (interaction-environment)
  (vector *env-tag*))

(define (scheme-report-environment version)
  (interaction-environment))

(define (null-environment)
  (interaction-environment))

(define (environment-lookup-binding env sym)
  (toplevel-cell sym))

;This is not usually useful?
;(define (environment-lookup-value env sym)
;  (toplevel-env-lookup sym))

; Global cells are currently represented as pairs.

(define make-global-cell cons)
(define global-cell-ref car)
(define global-cell-set! set-car!)

; eof
