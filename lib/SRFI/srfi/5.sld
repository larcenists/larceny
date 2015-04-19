;; SRFI-5: A compatible let form with signatures and rest arguments
;; Reference implementation
;;
;; $Id$
;;
;; Conflicts with (rnrs base): let
;;

(define-library (srfi 5 let)

  (export let)

  (import (srfi :5 let)))

(define-library (srfi 5)
  (export let)
  (import (srfi 5 let)))

; eof
