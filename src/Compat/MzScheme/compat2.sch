;; Since we already have this file, Felix is going to add content to
;; it that parallels the content he is adding to Larceny's compat
;; files.

;; Felix thinks the main distinction between compat.sch and
;; compat2.scm in the Larceny case is that compat.sch is not compiled
;; to a fasl (and thus can export syntax definitions, though as of
;; 2009jul03 it does not) while compat2.sch is explicitly compiled to
;; a fasl file, and thus should not contain any syntax definitions
;; intended for export.

;; A Predicate[X] is a (Any -> Boolean) that produces #t for instances
;; of X and a #f for all other values.

;; twobit-iterative-try/fallback 
;;    : X (X -> Y) Predicate[Z] (X Z -> X) (X -> Y) -> Y
;; Invokes try on input repeatedly, responding to exceptional condition Z
;; by attempting to revise the input (passing along  and try again.
;; No matter what, the system should behave sanely (if sub-optimally)
;; when last-resort is invoked on (revise^n input) for all n in Nat.

(define (twobit-iterative-try/fallback input try fail? revise last-resort)
  ;; the fallback case should always be a reasonable default.
  (last-resort input))

;; filter : (X -> Bool) [Listof X] -> [Listof X]
;; returns list containing only elements from LST satisfying PRED?

(require (rename (lib "1.ss" "srfi") filter filter))
