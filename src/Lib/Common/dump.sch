; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- heap dumping

($$trace "dump")

(define (dump-heap filename proc)
  (cond ((not (string? filename))
	 (error "dump-heap: invalid file name: " filename)
	 #t)
	((not (procedure? proc))
	 (error "dump-heap: invalid procedure: " proc)
	 #t)
	(else
	 (display "; Dumping heap...") (newline)
         (reset-all-hashtables!)
	 (sys$dump-heap filename 
			(lambda (argv)
			  (command-line-arguments argv)
			  (run-init-procedures)
			  (proc argv)))
	 (display "; Done.")
	 (newline))))

; eof
