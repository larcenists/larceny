; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on Unix systems,
; modulo certain platform issues like endianness.
;
; Loaded from petit-unix-el.sch and petit-unix-be.sch.

(define nbuild-parameter #f)
(define *requires-shared-runtime* #f)

(define (load-compiler)
  (load (make-filename "" "Util" "nbuild.sch"))
  (configure-system)
  (unspecified))

(define (common-unix-initialize)
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch")))

(define (setup-directory-structure)
  (case (nbuild-parameter 'host-os)
    ((unix)
     (system "mkdir Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os)))))

(define (build-config-files)

  (define (maybe-file fn)
    (if (file-exists? fn) (list fn) '()))

  (case (nbuild-parameter 'host-os)
    ((unix)
     (system "cp Rts/*.cfg Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os))))
  (expand-file "Rts/Standard-C/arithmetic.mac" "Rts/Standard-C/arithmetic.c")
  (config "Rts/Build/except.cfg" (nbuild-parameter 'target-machine))
  (config "Rts/Build/layouts.cfg" (nbuild-parameter 'target-machine))
  (config (string-append "Rts/Build/" (nbuild-parameter 'globals-table))
	  (nbuild-parameter 'target-machine))
  (config "Rts/Build/mprocs.cfg" (nbuild-parameter 'target-machine))
  (catfiles '("Rts/Build/globals.ch"
	      "Rts/Build/except.ch"
	      "Rts/Build/layouts.ch"
	      "Rts/Build/mprocs.ch")
	    "Rts/Build/cdefs.h")
  (catfiles '("Rts/Build/globals.sh" 
	      "Rts/Build/except.sh" 
	      "Rts/Build/layouts.sh")
	    "Rts/Build/schdefs.h")
  (catfiles (append (maybe-file "Rts/Build/globals.ah")
		    (maybe-file "Rts/Build/except.ah")
		    (maybe-file "Rts/Build/layouts.ah"))
	    "Rts/Build/asmdefs.ah")
  (load "features.sch"))

(define (build-heap . args)
  (apply make-petit-heap args))	     ; Defined in Lib/makefile.sch

(define (build-runtime)
  (if *requires-shared-runtime*
      (execute-in-directory "Rts" "make libpetit.so")
      (execute-in-directory "Rts" "make libpetit.a")))

(define (build-petit)
  (build-application (petit-application-name) '()))

(define (build-twobit)
  (make-petit-development-environment)
  (build-application (twobit-application-name)
		     (petit-development-environment-lop-files)))

(define (require-shared-runtime!)
  (if (not *requires-shared-runtime*)
      (begin
        (set! *requires-shared-runtime* #t)
        (set! unix/petit-rts-library "Rts/libpetit.so"))))

(define (remove-runtime-objects)
  (system "rm -f Rts/libpetit.a")
  (system "rm -f Rts/libpetit.so")
  (system "rm -f Rts/Sys/*.o")
  (system "rm -f Rts/Standard-C/*.o")
  (system "rm -f Rts/Build/*.o")
  #t)

(define (remove-heap-objects . extensions)
  (let ((ext   '("o" "c" "lap" "lop" "asm"))
	(names '(obj c lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "rm -f petit petit.o petit.heap libheap.a")
    (for-each (lambda (ext)
		(for-each (lambda (dir) 
			    (system (string-append "rm -f " dir "*." ext))) 
			  (list (nbuild-parameter 'common-source)
				(nbuild-parameter 'machine-source)
				(nbuild-parameter 'repl-source)
				(nbuild-parameter 'interp-source)
				(nbuild-parameter 'compiler)
                                (nbuild-parameter 'auxiliary))))
	      ext)
    #t))

(define (execute-in-directory dir cmd)
  (system (string-append "( cd " dir "; " cmd " )" )))

(define (catfiles input-files output-file)
  (if (not (null? input-files))
      (system (string-append "cat " 
			     (apply string-append 
				    (map (lambda (x) 
					   (string-append x " "))
					 input-files))
			     " > " 
			     output-file))
      (system (string-append "cat /dev/null > " output-file))))

(define (compile-files infilenames outfilename)
  (let ((user      (assembly-user-data))
	(syntaxenv (syntactic-copy (the-usual-syntactic-environment)))
	(segments  '())
	(c-name    (rewrite-file-type outfilename ".fasl" ".c"))
	(o-name    (rewrite-file-type outfilename ".fasl" ".o"))
	(so-name   (rewrite-file-type outfilename ".fasl" ".so")))
    (for-each (lambda (infilename)
		(call-with-input-file infilename
		  (lambda (in)
		    (do ((expr (read in) (read in)))
			((eof-object? expr))
		      (set! segments 
			    (cons (assemble (compile expr syntaxenv) user) 
				  segments))))))
	      infilenames)
    (let ((segments (reverse segments)))
      (delete-file c-name)
      (delete-file o-name)
      (delete-file so-name)
      (create-loadable-file outfilename segments so-name)
      (c-link-shared-object so-name (list o-name) '())
      (unspecified))))

(define (install-twobit basedir)
  (let ((incdir (make-filename basedir "include"))
	(libdir (make-filename basedir "lib")))
    (for-each (lambda (fn)
		(if (file-exists? fn)
		    (system (string-append "cp " fn " " incdir))))
	      '("Rts/Standard-C/petit-instr.h"
		"Rts/Standard-C/millicode.h"
		"Rts/Standard-C/petit-config.h"
		"Rts/Standard-C/petit-machine.h"
		"Rts/Sys/larceny-types.h"
		"Rts/Sys/macros.h"
		"Rts/Sys/assert.h"
		"Rts/Build/config.h"
		"Rts/Build/cdefs.h"
		"Rts/Build/asmdefs.h"))
    (system (string-append "cp libheap.a " libdir))
    (if (file-exists? "Rts/libpetit.a")
        (system (string-append "cp Rts/libpetit.a " libdir)))
    (if (file-exists? "Rts/libpetit.so")
        (system (string-append "cp Rts/libpetit.so " libdir)))
    (if (file-exists? "Rts/libpetit.dylib")
        (system (string-append "cp Rts/libpetit.dylib " libdir)))
    (set! unix/petit-include-path (string-append "-I" incdir))
    ; Note order here, .so overrides .a
    (if (file-exists? "Rts/libpetit.a")
        (set! unix/petit-rts-library (string-append libdir "/libpetit.a")))
    (if (file-exists? "Rts/libpetit.so")
        (set! unix/petit-rts-library (string-append libdir "/libpetit.so")))
    (if (file-exists? "Rts/libpetit.dylib")
        (set! unix/petit-rts-library (string-append libdir "/libpetit.dylib")))
    (set! unix/petit-lib-library (string-append libdir "/libheap.a"))
    'installed))

; eof
