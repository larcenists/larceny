;; 'type' is ".fasl" or ".efasl"

(define (compilecompiler type)
  (for-each (lambda (x)
	      (display "Compiling ") (display x) (newline)
	      (compile-file (string-append "Compiler/" x ".sch")
			    (string-append "Compiler/" x type)))
	    '("pass1" "pass1.aux" "pass1.imp" "pass2" "pass2.aux"
		      "pass4p1" "pass4p2" "pass4.aux" "pass4.imp" "assembler"))
		      
  (for-each (lambda (x)
	      (display "Compiling ") (display x) (newline)
	      (compile-file (string-append "Sparc/" x ".sch")
			    (string-append "Sparc/" x type)))
	    '("sparcasm" "sparcdis" "gen-msi" "gen-prim"))

  #t)

(define (loadcompiler type)
  (load "Build/schdefs.h")
  (for-each (lambda (x)
	      (display "Loading ") (display x) (newline)
	      (load (string-append "Compiler/" x type)))
	    '("pass1" "pass1.aux" "pass1.imp" "pass2" "pass2.aux"
		      "pass4p1" "pass4p2" "pass4.aux" "pass4.imp" "assembler"))
		      
  (for-each (lambda (x)
	      (display "Loading ") (display x) (newline)
	      (load (string-append "Sparc/" x type)))
	    '("sparcasm" "sparcdis" "gen-msi" "gen-prim"))

  #t)
