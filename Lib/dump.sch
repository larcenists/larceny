; Lib/dump.sch
; Larceny library -- heap dumping
;
; $Id$

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
	 (sys$dump-heap filename 
			(lambda (argv)
			  (command-line-arguments argv)
			  (run-init-procedures)
			  (proc argv)))
	 (display "; Done.")
	 (newline))))

; eof
