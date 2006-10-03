; Simple REQUIRE system, once superficially compatible with SLIB's.
; 2004-01-13 / lth
;
; Packages are named by symbols and consist of multiple files which
; are located in package directories, also named by symbols.  Usually
; many packages are located in one such directory.
;
; (REQUIRE)
;    List the names of all available packages.
;
; (REQUIRE <symbol>)
;    Load the package for <symbol> if it has not already been loaded
;    Will load .sch and .scm files, and corresponding .fasl files if
;    they exist and are newer than the source.
;
; (REQUIRE-LOCATION)
;    List the names of all available directories and their full paths.
;
; (REQUIRE-LOCATION <symbol>)
;    Return a string representing the package directory named by the
;    symbol.

(define require)                        ; (require <symbol>)
(define require-location)               ; (require-location <symbol>)

; *require-paths*
;
; Association list from key to path

(define *require-paths*
  '((library      . "/home/lth/lib/larceny/schemelib/")
    (larceny      . "/home/lth/larceny/")
    (srfi         . (library "srfi/"))
    (experimental . (larceny "Experimental/"))
    (debugger     . (larceny "Debugger/"))
    (ffi          . (larceny "Ffi/"))))
  

; *require-database*
;
; List of (package-name directory-key files loaded?)
;
; Only defined if it is not already defined, so that REQUIRE can be
; included in a dumped heap image.

(if (not (environment-variable? (interaction-environment) '*require-database*))
    (set! *require-database* '()))


; (register-package package-name directory-key files)
;
; Add package to the database.  Will change the database if the
; directory key or file list has changed, but will not change the
; loaded? flag for the package even if the file list has changed.

(define (register-package name dir files)
  (let ((probe (assq name *require-database*))
	(files (if (list? files) files (list files))))
    (if probe
	(if (or (not (equal? files (caddr probe)))
		(not (eq? dir (cadr probe))))
	    (begin
	      (set-car! (cdr probe) dir)
	      (set-car! (cddr probe) files)))
	(set! *require-database* 
	      (cons (list name dir files #f)
		    *require-database*)))))

; Database initialization

(map (lambda (x)
       (if (symbol? x)
	   (register-package x 'library (symbol->string x))
	   (register-package (car x) 'library (cadr x))))
     '(apply-hook apropos array array-util assert box brackets combinatory 
       comment common-syntax control coroutine debugger define-record 
       define-values defmacro dotimes exec-comment ffi file-system 
       file-utils fluid format fortune fqueue generator glob infix-expr 
       io list list-set (match ("match" "match-syntax")) md5 
       nonblocking-console number poll pretty queue random record 
       regexp sharp-dot shivers-syntax socket stats string symbol tasking
       tasking-with-io time trie unify unix word))

(map (lambda (x)
       (register-package x 'srfi (symbol->string x)))
     '(srfi-0 srfi-1 srfi-2 srfi-5 srfi-6 srfi-7 srfi-8 srfi-9 srfi-11
       srfi-13 srfi-14 srfi-16 srfi-17 srfi-19 srfi-23 srfi-25 srfi-26
       srfi-27 srfi-28 srfi-29 srfi-30 srfi-31 srfi-37 srfi-38 srfi-39 
       srfi-42))

(map (lambda (x)
       (register-package (car x) 'experimental (cadr x)))
     '((experimental/circular            ("read-circular" "print-circular"))
       (experimental/iosys               "iosys")
       (experimental/transcript          "transcript")
       (experimental/poll                "poll")
       (experimental/unix                "unix")
       (experimental/unix-descriptor     "unix-descriptor")
       (experimental/socket              "socket")
       (experimental/tasking             "tasking")
       (experimental/tasking-unix        "tasking-unix")
       (experimental/nonblocking-console "nonblocking-console")))

(register-package 'debugger-internals 'debugger
		  '("debug" "inspect-cont" "trace"))

(register-package 'ffi-internals 'ffi "ffi-load")

(let ((load load))

  (define (load-file fn)

    (define (load-file fn)
      (load fn)
      (display "; Loaded ")
      (display fn)
      (newline))

    (let ((fasl (string-append fn ".fasl"))
          (sch (string-append fn ".sch"))
          (scm (string-append fn ".scm")))
      (let ((src (cond ((and (file-exists? sch) (file-exists? scm))
                        (error "Require is finding both " sch 
                               " and " scm "."))
                       ((file-exists? scm) scm)
                       (else sch))))
        (cond ((and (file-exists? fasl) (file-exists? src))
               (if (file-newer? fasl src)
                   (load-file fasl)
                   (load-file src)))
              ((file-exists? src)
               (load-file src))
              ((file-exists? fasl)
               (load-file fasl))
              (else
               (error "Can't find either \"" src "\" or \"" fasl "\"."))))))

  (define (load-package key)
    (let ((probe (assq key *require-database*)))
      (if (not probe)
          (error "REQUIRE: " key " is not a valid key.")
          (if (not (provided? key))
              (let ((done #f))
                (dynamic-wind
                 (lambda () 
                   (provide key))
                 (lambda () 
                   (call-without-interrupts ; Hm, dangerous?
                     (lambda ()
		       (let ((path (get-path (cadr probe))))
			 (for-each (lambda (f) 
				     (load-file (string-append path f)))
				   (caddr probe))
			 (set! done #t))
		       (unspecified))))
                 (lambda ()
                   (if (not done)
                       (unprovide key)))))))))

  (define (get-path key)
    (let ((probe (assq key *require-paths*)))
      (if (string? (cdr probe))
	  (cdr probe)
	  (apply string-append (map (lambda (x)
				      (if (string? x)
					  x
					  (get-path x)))
				    (cdr probe))))))
 
  (define (provide key)
    (let ((probe (assq key *require-database*)))
      (if probe
	  (set-car! (cdddr probe) #t))))
  
  (define (unprovide key)
    (let ((probe (assq key *require-database*)))
      (if probe
	  (set-car! (cdddr probe) #f))))

  (define (provided? key)
    (let ((probe (assq key *require-database*)))
      (and probe (cadddr probe))))

  (set! require 
	(lambda args
	  (if (null? args)
	      (map car *require-database*)
	      (apply load-package args))))

  (set! require-location
	(lambda args
	  (if (null? args)
	      (map (lambda (x)
		     (cons x (get-path x)))
		   (map car *require-paths*))
	      (get-path (car args)))))

  'require)

; eof
