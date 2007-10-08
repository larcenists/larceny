; Various platform-independent utilities for the build system.

(define (with-current-directory dir thunk)
  (let ((cdir #f))
    (dynamic-wind
	(lambda ()
	  (set! cdir (current-directory))
	  (current-directory dir))
	thunk
	(lambda ()
	  (set! dir (current-directory))
	  (current-directory cdir)))))

; File lists and procedures

(define (writeln . x)
  (for-each display x) (newline))

(define (code-cov-files) (append (nbuild:twobit-files)
                               (nbuild:common-asm-files)
                               (nbuild:machine-asm-files)))

(define (new-files)
  (map (lambda (x) (string-append x ".stcov")) (code-cov-files)))

(define (nbuild-load-files files)
  (for-each compat:load files))

(define (welcome)
  (newline)
  (writeln "Welcome. Type (help) for help."))

