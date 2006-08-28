; SRFI-28: Basic format strings
; lth@acm.org / 2004-01-10
;
; Larceny already has FORMAT built in, and a fancier one to boot.
;
; The built-in one accepts a port, #t, or #f for the first argument,
; so this one does too, while accepting a string as SRFI-28 requires.

(cond-expand (srfi-6))

(define format
  (let ((format format))
    (lambda (fst . args)
      (if (string? fst)
	  (let ((s (open-output-string)))
	    (apply format s fst args)
	    (get-output-string s))
	  (apply format fst args)))))
