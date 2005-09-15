(require (lib "process.ss"))

;; run-mzscheme/ci : String String -> Any
;; Runs mzscheme binary on args (already parsed, system* style),
;; in case insenstive mode.  This is tricky because versions
;; of mzscheme prior to v299 did not have a -G option,
;; and die when you pass it to them.
(define (run-mzscheme/ci path-to-mzscheme-cmd . args)
  (display `(run-mzscheme/ci ,path-to-mzscheme-cmd ,@args)) (newline)
  (display `(version: ,(version))) (newline)
  (cond ((< (string->number (version)) 299)
	 (apply system* path-to-mzscheme-cmd "-e" "(define (path->string x) x)" args))
	(else
	 (apply system* path-to-mzscheme-cmd "-G" args))))

;; Main method, for use when invoked via -C (aka --main) option to
;; mzscheme.  Automatically removes the name of this script and
;; attempts to supply the name of the mzscheme binary, for better or
;; for worse.
(define main
  (lambda (args)
    (write `(main ,@args)) (newline)
    (apply 
	 run-mzscheme/ci 
     (find-executable-path "MzScheme.exe" #f)
     (cdr args))))
