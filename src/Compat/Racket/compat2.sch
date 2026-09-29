; Second part of the Racket compatibility code.

;; twobit-iterative-try/fallback
;;    : X (X -> Y) Predicate[Z] (X Z -> X) (X -> Y) -> Y
;; See Compat/MzScheme/compat2.sch.

(define (twobit-iterative-try/fallback input try fail? revise last-resort)
  ;; the fallback case should always be a reasonable default.
  (last-resort input))

; eof
