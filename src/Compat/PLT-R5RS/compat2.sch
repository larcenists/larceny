;; Felix is *not* loading ../MzScheme/compat2.sch, because it is too
;; much of a nightmare to deal with the PLT 4.x [imm]mutable list
;; issues.  (But that does mean you need to keep the two files in sync.)

;; -------------------------------------------------------------------------

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

;; filter : (X -> Bool) [MListof X] -> [MListof X]
;; returns list containing only elements from LST satisfying PRED?

(define (filter p? l)
  ;; (if someone finds a standard mlist-filter or mfilter in PLT libs,
  ;;  feel free to relace this def'n with appropriate require.)
  (let loop ((l l))
    (cond ((null? l) l)
          ((p? (car l)) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))))
