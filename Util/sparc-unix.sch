; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building SPARC-native Larceny on generic Unix
; systems, under Larceny.

(define nbuild-parameter #f)

(define (unix-initialize)
  (load "Util/sysdep-unix.sch")
  (load "Util/nbuild-param.sch")
  (set! nbuild-parameter 
	(make-nbuild-parameter 'always-source? #f
                               'verbose-load? #t
                               'development? #t
                               'machine-source "Lib/Sparc/"
                               'host-os 'unix
                               'host-endianness 'big
                               'target-machine 'SPARC
                               'target-os 'unix
                               'target-endianness 'big))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (unspecified))

(define (setup-directory-structure)
  (case (nbuild-parameter 'host-os)
    ((unix)
     (system "mkdir Rts/Build"))
    (else
     (error "Unknown host OS " (nbuild-parameter 'host-os)))))

(define (build-makefile . rest)
  (let ((c (if (null? rest)
	       'sparc-solaris-static-gcc
	       (car rest))))
    (generate-makefile "Rts/Makefile" c)))

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
  (catfiles '("Rts/Build/globals.ah"
              "Rts/Build/except.ah"
              "Rts/Build/layouts.ah"
              "Rts/Build/mprocs.ah"
              "Rts/Build/regs.ah")
	    "Rts/Build/asmdefs.h")
  (load "features.sch"))

(define (build-heap . args)
  (apply make-sparc-heap args))	     ; Defined in Lib/makefile.sch

(define (build-runtime)
  (delete-file "larceny.bin")
  (execute-in-directory "Rts" "make larceny.bin")
  (if (file-exists? "Rts/larceny.bin")
      (system "cp Rts/larceny.bin .")))

(define (build-twobit)
  (make-development-environment))

(define (load-compiler . rest)
  (if (and (not (null? rest)) (eq? (car rest) 'release))
      (begin
        (nbuild-parameter 'always-source? #f)
        (nbuild-parameter 'verbose-load? #f)
        (nbuild-parameter 'development? #f)))
  (load (make-filename "" "Util" "nbuild.sch"))
  (welcome)
  (unspecified))

(define (remove-runtime-objects)
  (system "rm -f Rts/Sys/*.o")
  (system "rm -f Rts/Sparc/*.o")
  (system "rm -f Rts/Build/*.o")
  (unspecified))

(define (remove-heap-objects . extensions)
  (let ((ext   '("lap" "lop" "fasl"))
	(names '(lap lop fasl)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (system "rm -f larceny.bin Rts/larceny.bin sparc.heap")
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
    (unspecified)))

(unix-initialize)

(define (execute-in-directory dir cmd)
  (system (string-append "( cd " dir "; " cmd " )" )))

; eof
