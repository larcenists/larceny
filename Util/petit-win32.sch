; 1 August 2004
;
; General "script" for building Petit Larceny on Win32 systems, using
; Win32-native C compilers like MinGW, CodeWarrior and Visual C/C++,
; and hosting the build under Larceny.
;
; See Docs/HOWTO-PETIT for more information.

(define nbuild-parameter #f)

(define make-command "nmake")

(define (win32-initialize)
  (load "Util\\sysdep-win32.sch")
  (load "Util\\Configurations\\nbuild-param-C-el-win32.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter "" #f #t #t "Larceny" "Petit Larceny"))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (set! config-path "Rts\\Build\\")
  #t)

(define (setup-directory-structure)
  (case (nbuild-parameter 'host-os)
    ((win32)
     (system "mkdir Rts\\Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os)))))

(define (build-config-files)
  (case (nbuild-parameter 'host-os)
    ((win32)
     (system "copy Rts\\*.cfg Rts\\Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os))))
  (expand-file "Rts\\Standard-C\\arithmetic.mac"
	       "Rts\\Standard-C\\arithmetic.c")
  (config "Rts\\Build\\except.cfg")
  (config "Rts\\Build\\layouts.cfg")
  (config "Rts\\Build\\globals.cfg")
  (config "Rts\\Build\\mprocs.cfg")
  (catfiles '("Rts\\Build\\globals.ch"
	      "Rts\\Build\\except.ch"
	      "Rts\\Build\\layouts.ch"
	      "Rts\\Build\\mprocs.ch")
	    "Rts\\Build\\cdefs.h")
  (catfiles '("Rts\\Build\\globals.sh" 
	      "Rts\\Build\\except.sh" 
	      "Rts\\Build\\layouts.sh")
	    "Rts\\Build\\schdefs.h")
  (load "features.sch"))

(define (catfiles input-files output-file)
  (delete-file output-file)
  (call-with-output-file output-file
    (lambda (out)
      (for-each (lambda (f)
		  (call-with-input-file f
		    (lambda (in)
		      (do ((c (read-char in) (read-char in)))
			  ((eof-object? c))
			(write-char c out)))))
		input-files))))

(define (build-heap . args)
  (apply make-petit-heap args))	     ; Defined in Lib/makefile.sch

(define (build-runtime)
  (execute-in-directory 
   "Rts" 
   (string-append make-command " petit-rts" (lib-suffix))))

(define build-runtime-system build-runtime) ; Old name

(define (build-petit)
  (build-application "petit.exe" '()))

(define (build-twobit)
  (make-petit-development-environment)
  (build-application "twobit.exe" (petit-development-environment-lop-files)))

(define (build-development-system dll-name)
  (let ((files '())
	(old-compile-file compile-file))

    (define (new-compile-file infilename . rest)
      (set! files (cons infilename files)))

    (dynamic-wind
	(lambda ()
	  (set! compile-file new-compile-file))
	(lambda ()
	  (make-development-environment))
	(lambda ()
	  (set! compile-file old-compile-file)))

    (compile-files (reverse files) dll-name)))

(define (load-compiler)
  (load "Util\\nbuild.sch")
  (set! win32/petit-rts-library (string-append "Rts\\petit-rts" (lib-suffix)))
  (set! win32/petit-lib-library (string-append "petit-lib" (lib-suffix)))
  (unspecified))

(define (lib-suffix)
  (if (string=? (obj-suffix) ".o")
      ".a"
      ".lib"))

(define (remove-runtime-objects)
  (system (string-append "del Rts\\petit-rts" (lib-suffix)))
  (system "del Rts\\vc60.pdb")
  (system (string-append "del Rts\\Sys\\*" (obj-suffix)))
  (system (string-append "del Rts\\Standard-C\\*" (obj-suffix)))
  (system (string-append "del Rts\\Build\\*" (obj-suffix)))
  #t)

(define remove-rts-objects remove-runtime-objects) ; Old name

(define (remove-heap-objects . extensions)
  (let ((ext   '("obj" "o" "c" "lap" "lop"))
	(names '(obj c lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "del petit.exe")
    (system (string-append "del petit" (obj-suffix)))
    (system "del petit.pdb")
    (system "del petit.heap")
    (system (string-append "del petit-lib" (lib-suffix)))
    (system "del petit-lib.pdb")
    (system "del vc60.pdb")
    (for-each (lambda (ext)
		(for-each (lambda (dir) 
			    (system (string-append "del " dir "*." ext))) 
			  (list (nbuild-parameter 'common-source)
				(nbuild-parameter 'machine-source)
				(nbuild-parameter 'repl-source)
				(nbuild-parameter 'interp-source)
				(nbuild-parameter 'compiler)
                                (nbuild-parameter 'auxiliary))))
	      ext)
    #t))

(win32-initialize)

(define (execute-in-directory dir cmd)
  (with-current-directory dir
    (lambda ()
      (system cmd))))

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

(define (compile-files infilenames outfilename)
  (let ((user      (assembly-user-data))
	(syntaxenv (syntactic-copy (the-usual-syntactic-environment)))
	(segments  '())
	(c-name    (rewrite-file-type outfilename ".fasl" ".c"))
	(o-name    (rewrite-file-type outfilename ".fasl" (obj-suffix)))
	(so-name   (rewrite-file-type outfilename ".fasl" ".dll")))
    (for-each (lambda (infilename)
		(call-with-input-file infilename
		  (lambda (in)
		    (do ((expr (read in) (read in)))
			((eof-object? expr))
		      (set! segments (cons (assemble (compile expr syntaxenv)
						     user) 
					   segments))))))
	      infilenames)
    (set! segments (reverse segments))
    (create-loadable-file outfilename segments so-name)
    (c-link-shared-object so-name 
			  (list o-name) 
			  (list (string-append "Rts/petit-rts" (lib-suffix))))
    (unspecified)))

; eof
