; Util/init-comp.sch
; Code that initializes twobit after loading with load-environment.
;
; $Id$

; FIXME -- not right for e.g. standard-C.

(let ((f (system-features)))
  (cond ((string=? (cdr (assq 'operating-system-name f)) "SunOS")
	 (twobit-target-architecture 'SPARC))
	(else
	 (twobit-target-architecture 'unknown))))

(issue-warnings #f)			; Annoying
(include-source-code #f)		; Conserve space
(include-variable-names #t)		; Debugging
(include-procedure-names #t)		; Debugging
(fast-safe-code)			; Performance

; eof
