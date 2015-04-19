;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Library added for Larceny.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (larceny deprecated)             ; [Larceny]
  (export issue-warning-deprecated
          issue-deprecated-warnings?)
  (import
   (primitives
    issue-deprecated-warnings?
    issue-warning-deprecated)))

