; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building SPARC-native Larceny on generic Unix
; systems, under Larceny.
;
; This program is a self-contained development environment; it replaces
; the Unix shell scripts and the Util/Configurations/load-*.sch programs;

(define nbuild-parameter #f)

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/Configurations/nbuild-param-sparc.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter "" #t #t #t "Larceny" "Larceny"))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (set! config-path "Rts/Build/")
  #t)

(define (setup-directory-structure)
  (case (nbuild-parameter 'host-os)
    ((unix)
     (system "mkdir Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os)))))

(define (build-config-files)

  (define (catfiles input-files output-file)
    (system (string-append "cat " 
			   (apply string-append 
				  (map (lambda (x) 
					 (string-append x " "))
				       input-files))
			   " > " 
			   output-file)))

  (case (nbuild-parameter 'host-os)
    ((unix)
     (system "cp Rts/*.cfg Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os))))
  (config "Rts/Build/except.cfg")
  (config "Rts/Build/layouts.cfg")
  (config "Rts/Build/globals.cfg")
  (config "Rts/Build/mprocs.cfg")
  (config "Rts/Build/regs.cfg")
  (catfiles '("Rts/Build/globals.ch"
	      "Rts/Build/except.ch"
	      "Rts/Build/layouts.ch"
	      "Rts/Build/mprocs.ch")
	    "Rts/Build/cdefs.h")
  (catfiles '("Rts/Build/globals.sh" 
	      "Rts/Build/except.sh" 
	      "Rts/Build/layouts.sh"
	      "Rts/Build/regs.sh")
	    "Rts/Build/schdefs.h")
  (load "features.sch"))

(define (build-heap . args)
  (apply make-sparc-heap args))	     ; Defined in Lib/makefile.sch

(define (build-runtime)
  (build-executable))

(define build-runtime-system build-runtime)  ; Old name

(define (build-executable)
  (execute-in-directory "Rts" "make larceny.bin")
  (newline)
  (display "The program is Rts/larceny.bin")
  (newline))

(define (build-twobit)
  (error "Not yet implemented")
  (make-development-environment)
  (build-application "twobit"
		     (petit-development-environment-lop-files)))

(define (load-compiler)
  (load (make-filename "" "Util" "nbuild.sch")))

(define (remove-runtime-objects)
  (system "rm -f Rts/Sys/*.o")
  (system "rm -f Rts/Sparc/*.o")
  (system "rm -f Rts/Build/*.o")
  #t)

(define remove-rts-objects remove-runtime-objects)  ; Old name

(define (remove-heap-objects . extensions)
  (let ((ext   '("lap" "lop"))
	(names '(lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "rm -f Rts/larceny.bin sparc.heap")
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

; eof
