; $Id$
;
; General script for building Larceny on x86 using the NASM backend,
; under (Petit) Larceny on Unix.

(define nbuild-parameter #f)

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/Configurations/nbuild-param-nasm-unix.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter "" #t #t #t "Larceny" "Petit Larceny"))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (set! config-path "Rts/Build/")
  #t)

(define (setup-directory-structure)
  (system "mkdir Rts/Build"))

(define (build-config-files)

  (define (catfiles input-files output-file)
    (system (string-append "cat " 
			   (apply string-append 
				  (map (lambda (x) (string-append x " ")) 
				       input-files))
			   " > " 
			   output-file)))

  (system "cp Rts/*.cfg Rts/Build")
  (expand-file "Rts/Standard-C/arithmetic.mac" "Rts/Standard-C/arithmetic.c")
  (config "Rts/Build/except.cfg" 'x86-nasm)
  (config "Rts/Build/layouts.cfg" 'x86-nasm)
  (config "Rts/Build/globals-nasm.cfg" 'x86-nasm)
  (config "Rts/Build/mprocs.cfg" 'x86-nasm)
  (catfiles '("Rts/Build/globals.ch"
	      "Rts/Build/except.ch"
	      "Rts/Build/layouts.ch"
	      "Rts/Build/mprocs.ch")
	    "Rts/Build/cdefs.h")
  (catfiles '("Rts/Build/globals.ah"
	      "Rts/Build/except.ah"
	      "Rts/Build/layouts.ah")
	    "Rts/Build/asmdefs.ah")
  (catfiles '("Rts/Build/globals.sh" 
	      "Rts/Build/except.sh" 
	      "Rts/Build/layouts.sh")
	    "Rts/Build/schdefs.h")
  (load "features.sch"))

(define (build-heap . args)
  (apply make-petit-heap args))	     ; Defined in Lib/makefile.sch

(define (build-runtime)
  (execute-in-directory "Rts" "make libx86.a"))

(define build-runtime-system build-runtime)  ; Old name

(define (build-executable)
  (build-application "petit" '()))

(define (build-twobit)
  (make-petit-development-environment)
  (build-application "twobit" (petit-development-environment-lop-files)))

(define (load-compiler)
  (load (make-filename "" "Util" "nbuild.sch"))
  ; These vars are picked up by Asm/Standard-C/dumpheap-unix.sch
  (set! unix/petit-rts-library "Rts/libx86.a")
  (set! unix/petit-lib-library "libx86heap.a")
  (set! unix/petit-lib-library-platform 
	'("/usr/lib/libm.a" "/usr/lib/libdl.a"))
  (unspecified))

(define (remove-runtime-objects)
  (system "rm -f Rts/libx86.a")
  (system "rm -f Rts/Sys/*.o")
  (system "rm -f Rts/Standard-C/*.o")
  (system "rm -f Rts/Intel/*.o")
  (system "rm -f Rts/Build/*.o")
  #t)

(define remove-rts-objects remove-runtime-objects) ; Old name

(define (remove-heap-objects . extensions)
  (let ((ext   '("o" "asm" "lap" "lop"))
	(names '(obj c lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "rm -f petit petit.o petit.heap libx86heap.a")
    (for-each (lambda (ext)
		(for-each (lambda (dir) 
			    (system (string-append "rm -f " dir "*." ext))) 
			  (list (nbuild-parameter 'common-source)
				(nbuild-parameter 'machine-source)
				(nbuild-parameter 'repl-source)
				(nbuild-parameter 'interp-source)
				(nbuild-parameter 'compiler))))
	      ext)
    #t))

(unix-initialize)

(define (execute-in-directory dir cmd)
  (system (string-append "( cd " dir "; " cmd " )" )))

(define (compile-files infilenames outfilename)
  (let ((user      (assembly-user-data))
	(syntaxenv (syntactic-copy (the-usual-syntactic-environment)))
	; Doesn't work in Petit Larceny (yet, anyway)
	;(syntaxenv (syntactic-copy (environment-syntax-environment
	;			    (interaction-environment))))
	(segments  '())
	(c-name    (rewrite-file-type outfilename ".fasl" ".c"))
	(o-name    (rewrite-file-type outfilename ".fasl" ".o"))
	(so-name   (rewrite-file-type outfilename ".fasl" ".so")))
    (for-each (lambda (infilename)
		(call-with-input-file infilename
		  (lambda (in)
		    (do ((expr (read in) (read in)))
			((eof-object? expr))
		      (set! segments (cons (assemble (compile expr syntaxenv)
						     user) 
					   segments))))))
	      infilenames)
    (let ((segments (reverse segments)))
      (delete-file c-name)
      (delete-file o-name)
      (delete-file so-name)
      (create-loadable-file outfilename segments so-name)
      (c-link-shared-object so-name (list o-name) '())
      (unspecified))))

'(define (install-twobit basedir)
  (let ((incdir (make-filename basedir "include"))
	(libdir (make-filename basedir "lib")))
    (for-each (lambda (fn)
		(system (string-append "cp " fn " " incdir)))
	      '("Rts/Standard-C/twobit.h"
		"Rts/Standard-C/millicode.h"
		"Rts/Standard-C/petit-config.h"
		"Rts/Standard-C/petit-hacks.h"
		"Rts/Sys/larceny-types.h"
		"Rts/Sys/macros.h"
		"Rts/Sys/assert.h"
		"Rts/Sys/config.h"
		"Rts/Build/cdefs.h"))
    (for-each (lambda (fn)
		(system (string-append "cp " fn " " libdir)))
	      '("Rts/libx86.a"
		"libx86heap.a"))
    (set! unix/petit-include-path (string-append "-I" incdir))
    (set! unix/petit-rts-library (string-append libdir "/libx86.a"))
    (set! unix/petit-lib-library (string-append libdir "/libx86heap.a"))
    'installed))

; eof
