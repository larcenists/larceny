; -*- mode: scheme -*-
;
; $Id$
;
; General "script" for building Petit Larceny on generic Unix
; systems (including MacOS X), under arbitrary Scheme systems.
;
; This program is sets up parameters for a unified development
; environment; it unifies the petit-unix-be.sch and petit-unix-el.sch
; scripts.  
;
; See also petit-setup.sch, which sets parameters based on user input.
; and petit-unix-defns-globals.sch, which defines several globals
; used and mutated here and there.
(load "Util/petit-unix-defns-globals.sch")

(define (unix-initialize)
  (load (case *host:os*
	  ((unix macosx solaris linux-el) "Util/sysdep-unix.sch")
	  ((cygwin win32)      "Util/sysdep-win32.sch")
          (else (error 'unix-initialize "Must add support for host:os"))
	  ))
  (load "Util/nbuild-param.sch")
  (set! nbuild-parameter (make-nbuild-parameter 'always-source?    *always-source*
                                                'verbose-load?     #t
                                                'development?      #t
                                                'machine-source    (pathname-append "Lib" *target:machine-source*)
                                                'mzscheme-source   (pathname-append "Lib" "MzScheme")
                                                'host-os           *host:os*
                                                'host-endianness   *host:endianness*
                                                'target-machine    *target:machine*
                                                'target-os         *target:os*
                                                'target-endianness *target:endianness*
                                                'compatibility     (pathname-append "Compat" *host-dir*)
                                                'host-system       *host-name*))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (recognize-keywords? #f) ;; don't treat :NONE as (QUOTE NONE)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  #t)

(define (setup-directory-structure)
  (make-directory "Rts" "Build"))

(define (build-makefile . rest)
  (let ((c (cond (*makefile-configuration*)
                 ((null? rest) (default-makefile-configuration))
                 (else (car rest)))))
    (generate-makefile (make-filename "Rts" "Makefile") c)))

(define (build-config-files)
  (copy-file/regexp "Rts" "*.cfg" (pathname-append "Rts" "Build"))
  (expand-file "Rts/Standard-C/arithmetic.mac" "Rts/Standard-C/arithmetic.c")
  (config "Rts/Build/except.cfg")
  (config "Rts/Build/layouts.cfg")
  (config "Rts/Build/globals.cfg")
  (config "Rts/Build/mprocs.cfg")
  (config "Rts/Build/regs.cfg")    ; for Sparc (and Intel?) native
  (catfiles '("Rts/Build/globals.ch"
	      "Rts/Build/except.ch"
	      "Rts/Build/layouts.ch"
	      "Rts/Build/mprocs.ch")
	    "Rts/Build/cdefs.h")
  (catfiles '("Rts/Build/globals.sh" 
	      "Rts/Build/except.sh" 
	      "Rts/Build/layouts.sh"
              "Rts/Build/regs.sh") ; for Sparc native
	    "Rts/Build/schdefs.h")
  ;; for Sparc and Intel native
  (catfiles '("Rts/Build/globals.ah"
              "Rts/Build/except.ah"
              "Rts/Build/layouts.ah"
              "Rts/Build/mprocs.ah"
              "Rts/Build/regs.ah")
	    "Rts/Build/asmdefs.h")
  (load "features.sch")
  ;; [pnkfelix] loading features.sch calls DEFINE-FEATURE-SET, but I
  ;; think it is safe to leave that in there and just call it again
  ;; below (when necessary).

  ;; Note: *CHANGE-FEATURE-SET* defined as #f above; expression has no effect unless SETUP run
  (let-syntax ((feature-case (syntax-rules ()
                               ((feature-case ID ...)
                                (case *change-feature-set*
                                  ((ID) (set! selected-feature-set ID))
                                  ...)))))
    ;; Copied names from features.sch.  This code might be better off in that
    ;; file (where *CHANGE-FEATURE-SET* would be a parameter to DEFINE-FEATURE-SET)
    (feature-case features-sparc-solaris        ; solaris 2.5 or later
                  features-petit-solaris	; solaris 2.5 or later
                  features-petit-macosx		; gcc and GNU libc
                  features-petit-win32		; works for Mingw; believed to work
                  features-petit-linux		; Debian GNU/Linux 3.0 (woody), x86
                  features-petit-cygwin		; Tested with cygwin 1.5.10 (May 2004)
                  features-x86-nasm-linux       ; Debian GNU/Linux 3.0 (woody), x86
                  features-petit-linux-redhat5	; Very old, Redhat linux 5.1
                  features-sparc-linux-debian	; Very old, SPARC Debian v2(?)
                  features-petit-macos9-cw3     ; Very old (ca v0.48), CW Pro 3
                  features-petit-osf4-alpha     ; Very old, OSF/1 4.0 on DEC Alpha
                  ))

  (define-feature-set)
  )

(define (build-heap . args)
  (let ((make-heap (case *heap-type* ((petit) make-petit-heap) ((sparc-native) make-sparc-heap)) ))
    (apply make-heap args)))	     ; Defined in Lib/makefile.sch

;; adapted from petit-win32.sch
(define (make-command)
  (case *host:os*
    ((win32) (if (eq? 'gcc-mingw (compiler-tag (current-compiler)))
		 "mingw32-make"
		 "nmake"))
    (else "make")))


;; sparc-unix.sch copies the resulting larceny.bin executable to
;; current directory; do we want that?  Or perhaps that should be
;; something BUILD-EXECUTABLE does?
(define (build-runtime)
  (if (not (file-exists? "Rts/Makefile"))
      (build-makefile))

;;; pnkfelix: added the following command, to setup the runtime post
;;; calls into cleanup.sch.  The *right* way to do this would be to
;;; figure out how to encode the dependency a la make; this solution
;;; is fragile in that changes to arithmetic.mac will not be
;;; propagated without an intervening removal of arithmetic.c
  (if (not (file-exists? "Rts/Standard-C/arithmetic.c"))  
      (expand-file "Rts/Standard-C/arithmetic.mac" "Rts/Standard-C/arithmetic.c"))

  (let ((make-target (case *runtime-type* 
                       ((petit) 
			(case *host:os*
			  ((win32) "libpetit.lib")
			  (else    "libpetit.a")))
                       ((sparc-native) "larceny.bin"))))
    ;; petit-win32.sch actually doesn't pass an arg to make... should I do same?
    (execute-in-directory "Rts" (string-append (make-command) " " make-target))))

(define build-runtime-system build-runtime)  ; Old name

(define (build-executable)
  (case *runtime-type*
    ((petit)        (build-application *petit-executable-name* '()))
    ((sparc-native) (if (file-exists? "Rts/larceny.bin")
			(copy-file/regexp "Rts" "larceny.bin" ".")
                        (error "You need to build-runtime [in order to generate Rts/larceny.bin]")
                        ))))

(define (build-twobit)
  (make-petit-development-environment)
  (if (eq? 'petit *runtime-type*)
      (build-application *twobit-executable-name*
                         (petit-development-environment-lop-files))))

; Set up for loading Util/petit-r5rs-heap.sch
(define (build-r5rs-files)
  (case *heap-type*
    ((petit) 
     (compile-and-assemble313 "Auxlib/pp.sch")
     (build-application "petit-r5rs" '("Auxlib/pp.lop")))
    ((sparc-native)
     (compile-file "Auxlib/pp.sch"))
    (else (error 'build-r5rs-files "Unknown heap type"))))

; Set up for loading Util/petit-larceny-heap.sch
(define (build-larceny-files)
  (case *heap-type*
    ((petit)
     (make-petit-development-environment)
     (build-application "petit-larceny"
                        (petit-development-environment-lop-files)))
    ((sparc-native)
     (make-development-environment))
    (else (error 'build-larceny-files "Unknown heap type"))))
     

(define (is-macosx?)
  (string=? "MacOS X" (cdr (assq 'os-name (system-features)))))

(define (load-compiler . how)
  (if (not (null? how))
      (case (car how)
        ((release) ;; matching code in sparc-unix.sch
         (nbuild-parameter 'always-source? #f)
         (nbuild-parameter 'verbose-load? #f)
         (nbuild-parameter 'development? #f))
        ((development) ;; matching code in petit-unix-common.sch
         (nbuild-parameter 'development? #t))))
  (load (make-filename "Util" "nbuild.sch"))
  (if (eq? 'petit *heap-type*)
      (configure-system))
  (welcome)
  (unspecified))
  
(define (lib-suffix)
  (if (string=? (obj-suffix) ".o")
      ".a"
      ".lib"))

(define (shared-obj-suffix)
  (if (string=? (obj-suffix) ".o")
      ".so"
      ".dll"))

; Make a guess at any extra libraries used for the system.

(define (configure-system)
  (select-compiler (or *host:c-compiler*
		       (case *host:os* 
			 ((win32) 'msvc)
			 (else 'gcc))))

  (case *host:os*
    ((unix cygwin macosx linux-el)
     (let ((os-name (cdr (assq 'os-name (system-features)))))
       (set! unix/petit-lib-library-platform 
	     (cond ((string=? os-name "MacOS X") '())
		   ((string=? os-name "SunOS")   '("-lm -ldl"))
                   ((string=? os-name "Linux")   '("-lm -ldl"))
		   ((string=? os-name "Win32")   '())
		   (else                         '("-lm -ldl"))))))
    ((win32)
     (set! win32/petit-rts-library (string-append "Rts\\libpetit" (lib-suffix)))
     (set! win32/petit-lib-library (string-append "libheap" (lib-suffix))))
    )
  )

(define (remove-runtime-objects)
  (let ((libpetit.a (string-append "libpetit" (lib-suffix)))
	(*.o        (string-append "*" (obj-suffix))))
    (delete-file/regexp "Rts" libpetit.a)
    (delete-file/regexp "Rts" "vc*.pdb") ; from petit-win32.sch
    (delete-file/regexp (pathname-append "Rts" "Sys") *.o)
    (delete-file/regexp (pathname-append "Rts" "Standard-C") *.o)
    (delete-file/regexp (pathname-append "Rts" "Build") *.o)
    #t))

(define remove-rts-objects remove-runtime-objects)  ; Old name

(define (remove-heap-objects . extensions)
  (let ((delete-files (lambda (l)
			(for-each (lambda (f)
				    (delete-file/regexp "." f)) 
				  l)))
	(ext   (case *host:os* 
		 ((win32) '("obj" "o" "c" "lap" "lop"))
		 (else '("o" "c" "lap" "lop"))))
	(names '(obj c lap lop)))
    (if (not (null? extensions))
	(set! ext (apply append 
			 (map (lambda (n ext)
				(if (memq n extensions) (list ext) '()))
			      names
			      ext))))
    (delete-files `("petit" "petit.exe" ,(string-append "petit" (obj-suffix)) 
		    "petit.pdb"
		    "petit.heap" 
		    ,(string-append "libpetit" (lib-suffix))
		    "libpetit.pdb" ; win32
		    "vc60.pdb"     ; win32
		    ,(string-append "libheap" (lib-suffix))
		    ))
    (delete-file/regexp "Rts" "larceny.bin")
    (delete-files '("larceny.bin" "sparc.heap"))
    (for-each (lambda (ext)
		(for-each (lambda (dir) 
			    (delete-file/regexp dir (string-append "*." ext)))
			  (list (nbuild-parameter 'common-source)
				(nbuild-parameter 'machine-source)
				(nbuild-parameter 'repl-source)
				(nbuild-parameter 'interp-source)
				(nbuild-parameter 'compiler)
				(nbuild-parameter 'auxiliary)
                                )))
	      ext)
    #t))

;; Do NOT execute anything within the definitions file; 
;; instead we'll call it all from the setup procedure.
;; (unix-initialize)

(define (ensure-fresh-name filename)
  (define (helper filename num)
    (let ((filename* (string-append filename "." (number->string num 16))))
      (if (not (file-exists? filename*))
          filename*
          (helper filename (+ num 1)))))
  (if (not (file-exists? filename))
      filename
      (helper filename 0)))

; I think this works, but dynamic loading does not work on MacOS X 10.1.5,
; so I've been unable to test.

(define (compile-files infilenames outfilename . rest)
  (let ((user      (assembly-user-data))
	(syntaxenv (if (null? rest)
                       (syntactic-copy (the-usual-syntactic-environment))
                       (car rest)))
	; Doesn't work in Petit Larceny (yet, anyway)
	;(syntaxenv (syntactic-copy (environment-syntax-environment
	;			    (interaction-environment))))
	(segments  '())
	(c-name    (rewrite-file-type outfilename ".fasl" ".c"))
	(o-name    (rewrite-file-type outfilename ".fasl" (obj-suffix)))
	(so-name   (ensure-fresh-name
                    (rewrite-file-type outfilename ".fasl" (shared-obj-suffix)))))
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
      (delete-file c-name)  ; win32 doesn't do this
      (delete-file o-name)  ; or this
      (delete-file so-name) ; or this
      (create-loadable-file outfilename segments so-name)
      (c-link-shared-object so-name 
			    (list o-name) 
			    (case *host:os*
			      ((win32) (list (string-append "Rts/libpetit" (lib-suffix))))
			      (else '())))
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
