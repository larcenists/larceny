; -*- mode: scheme -*-
;
; 22 September 2002
;
; General "script" for building Petit Larceny on MacOS X systems, under Larceny.
;
; This program is a selfcontained development environment; it replaces the Unix shell
; scripts and the Util/load-*.sch programs;

(define nbuild-parameter #f)

(define *root-directory* "")
(define *sysdep-file* 
  (string-append *root-directory* "Util/sysdep-unix.sch"))

(define (macosx-initialize)
  (load *sysdep-file*)
  (load "Util/nbuild-param-C-be-macosx.sch")
  (set! nbuild-parameter (make-nbuild-parameter "" #f #t #t "Larceny" "Petit Larceny"))
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
  (case (nbuild-parameter 'host-os)
    ((macosx)
     (system "mkdir Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os)))))

(define (build-config-files)

  (define (catfiles input-files output-file)
    (system (string-append "cat " 
			   (apply string-append (map (lambda (x) (string-append x " ")) input-files))
			   " > " 
			   output-file)))

  (case (nbuild-parameter 'host-os)
    ((macosx)
     (system "cp Rts/*.cfg Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os))))
  (expand-file "Rts/Standard-C/arithmetic.mac" "Rts/Standard-C/arithmetic.c")
  (config "Rts/Build/except.cfg")
  (config "Rts/Build/layouts.cfg")
  (config "Rts/Build/globals.cfg")
  (config "Rts/Build/mprocs.cfg")
  (catfiles '("Rts/Build/globals.ch"
	      "Rts/Build/except.ch"
	      "Rts/Build/layouts.ch"
	      "Rts/Build/mprocs.ch")
	    "Rts/Build/cdefs.h")
  (catfiles '("Rts/Build/globals.sh" 
	      "Rts/Build/except.sh" 
	      "Rts/Build/layouts.sh")
	    "Rts/Build/schdefs.h"))

(define (build-runtime-system)
  (execute-in-directory "Rts" "make libpetit.a"))

(define (build-executable)
  (build-application "petit" '()))

(define (build-twobit)
  (make-petit-development-environment)
  (build-application "twobit" (petit-development-environment-lop-files)))

(define (load-compiler)
  (load (make-filename *root-directory* "Util" "nbuild.sch")))

(define (remove-rts-objects)
  (system "rm -f Rts/libpetit.a")
  (system "rm -f Rts/Sys/*.o")
  (system "rm -f Rts/Standard-C/*.o")
  (system "rm -f Rts/Build/*.o")
  #t)

(define (remove-heap-objects . extensions)
  (let ((ext   '("o" "c" "lap" "lop"))
	(names '(obj c lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "rm -f petit petit.o petit.heap libheap.a")
    (for-each (lambda (ext)
		(for-each (lambda (dir) (system (string-append "rm -f " dir "*." ext))) 
			  '("Lib/Common/"
			    "Lib/Standard-C/"
			    "Repl/"
			    "Interpreter/"
			    "Compiler/")))
	      ext)
    #t))

(macosx-initialize)

(define (execute-in-directory dir cmd)
  (system (string-append "( cd " dir "; " cmd " )" )))

; I think this works, but dynamic loading does not work on MacOS X 10.1.5,
; so I've been unable to test.

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
		      (set! segments (cons (assemble (compile expr syntaxenv) user) 
					   segments))))))
	      infilenames)
    (set! segments (reverse segments))
    (create-loadable-file outfilename segments so-name)
    (c-link-shared-object so-name (list o-name) '())
    (unspecified)))

; This is really the wrong thing because it creates one .c for all the files.
; Doing that is fine as such but it destroys incremental compilation.  Perhaps
; that's what we want...  We could generate LOP for all and then have a load
; step at the end?

'(define (compile-files infilenames so-name)
  (begin-shared-object so-name (string-append "\"" so-name "\""))
  (let ((user (assembly-user-data)))
    (do ((infilenames infilenames (cdr infilenames)))
	((null? infilenames))
      (let ((infilename (car infilenames)))
	(let ((syntaxenv   (syntactic-copy (the-usual-syntactic-environment)))
	      (segments2   '())
	      (outfilename (rewrite-file-type infilename ".sch" ".fasl")))
	  (display "Compiling ")
	  (display infilename)
	  (newline)
	  (call-with-input-file infilename
	    (lambda (in)
	      (do ((expr (read in) (read in)))
		  ((eof-object? expr)
		   (add-to-shared-object outfilename (reverse segments2)))
		(set! segments2 (cons (assemble (compile expr syntaxenv) user) 
				      segments2)))))))))
  (end-shared-object)
  (c-link-shared-object so-name 
			(list (rewrite-file-type so-name ".dll" ".obj"))
			'("Rts/petit-rts.lib"))
  (unspecified))

; eof
