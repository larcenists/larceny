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
(load "src/Build/petit-unix-defns-globals.sch")

(define (unix-&-win32-initialize)
  (load (case *host:os*
	  ((unix macosx macosx-el solaris linux-el) "src/Build/sysdep-unix.sch")
	  ((cygwin win32)      "src/Build/sysdep-win32.sch")
          (else (error 'unix-&-win32-initialize "Must add support for host:os"))
	  ))
  (load "src/Build/nbuild-param.sch")
  (set! nbuild-parameter (make-nbuild-parameter 'always-source?    *always-source*
                                                'verbose-load?     #t
                                                'development?      #t
                                                'machine-source    (pathname-append "src" "Lib" "Arch" *target:machine-source*)
                                                'mzscheme-source   (pathname-append "src" "Lib" "MzScheme")
                                                'host-os           *host:os*
                                                'host-endianness   *host:endianness*
                                                'target-machine    *target:machine*
                                                'target-os         *target:os*
                                                'target-endianness *target:endianness*
                                                'compatibility     (pathname-append "src" "Compat" *host-dir*)
						'globals-table     *globals-table*
                                                'host-system       *host-name*))
  (display "Loading ")
  (display (nbuild-parameter 'host-system))
  (display " compatibility package.")
  (newline)
  (load (param-filename 'compatibility "compat.sch"))
  (compat:initialize)
  (recognize-keywords? #f) ;; don't treat :NONE as (QUOTE NONE)
  (compat:load (param-filename 'util "expander.sch"))
  (compat:load (param-filename 'util "config.sch"))
  #t)

;; param-filename : Symbol x { String } x String           -> FilePath
;; param-filename : Symbol x { String } x (List-of String) -> (List-of FilePath)
(define (param-filename param . components)
  (let* ((reversed (reverse components))
         (init-r   (cdr reversed))
         (last     (car reversed))
         (lead     (nbuild-parameter param)))
   (define (one-file x)
     (apply make-filename lead (reverse (cons x init-r))))
   (cond
    ((string? last)  (one-file last))
    ((list? last)    (map one-file last)))))

(define (build-makefile . rest)
  (let ((c (cond (*makefile-configuration*)
                 ((null? rest) (default-makefile-configuration))
                 (else (car rest)))))
    (generate-makefile (param-filename 'rts "Makefile") c)))

(define (build-config-files)
  (define (in-rts . components)
    (apply param-filename 'rts components))
  (define (in-include . components)
    (apply param-filename 'include components))
  (expand-file (in-rts "Shared" "arithmetic.mac") (in-rts "Shared" "arithmetic.c"))
  (config (in-rts "except.cfg"))
  (config (in-rts "layouts.cfg"))
  (config (in-rts (nbuild-parameter 'globals-table)))
  (config (in-rts "mprocs.cfg"))
  ;; config'ing regs.cfg breaks x86-nasm.
  (if (eq? *runtime-type* 'sparc-native)
      (config (in-rts "regs.cfg")))
  (catfiles (map in-include
                 '("globals.ch"
                   "except.ch"
                   "layouts.ch"
                   "mprocs.ch"))
            (in-include "cdefs.h"))
  ;; for Sparc and Intel native
  (catfiles (map in-include
                 `("globals.ah"
                   "except.ah"
                   "layouts.ah"
                   "mprocs.ah"
                   ,@(if (eq? *runtime-type* 'sparc-native)
                       '("regs.ah")
                       '())))
            (in-include "asmdefs.h"))
  (compat:load (in-rts "features.sch"))

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
                  features-petit-macosx-el	; gcc and GNU libc
                  features-petit-win32		; works for Mingw; believed to work
                  features-petit-linux		; Debian GNU/Linux 3.0 (woody), x86
                  features-petit-cygwin		; Tested with cygwin 1.5.10 (May 2004)
                  features-x86-nasm-linux       ; Debian GNU/Linux 3.0 (woody), x86
                  features-x86-sassy-macosx
                  features-x86-sassy-linux
		  features-x86-nasm-win32
		  features-x86-sassy-win32
                  features-petit-linux-redhat5	; Very old, Redhat linux 5.1
                  features-sparc-linux-debian	; Very old, SPARC Debian v2(?)
                  features-petit-macos9-cw3     ; Very old (ca v0.48), CW Pro 3
                  features-petit-osf4-alpha     ; Very old, OSF/1 4.0 on DEC Alpha
                  ))

  (define-feature-set)
  )

(define (build-heap . args)
  (let ((make-heap (case *heap-type* 
		     ((petit) make-petit-heap) 
		     ((sassy) make-sasstrap-heap)
		     ((sparc-native) make-sparc-heap)) ))
    (apply make-heap args)))	     ; Defined in Lib/makefile.sch

;; adapted from petit-win32.sch
(define (make-command)
  (case *host:os*
    ((win32) (if (eq? 'gcc-mingw (compiler-tag (current-compiler)))
		 "mingw32-make"
		 "nmake"))
    (else "make")))


(define (copy-script name)
  (if (eq? *host:os* 'win32)
    (copy-file/regexp (param-filename 'util "Scripts") "larceny.bat" (string-append name ".bat"))
    (copy-file/regexp (param-filename 'util "Scripts") "larceny.sh" name)))

;; sparc-unix.sch copies the resulting larceny.bin executable to
;; current directory; do we want that?  Or perhaps that should be
;; something BUILD-EXECUTABLE does?
(define (build-runtime)
  (if (not (file-exists? (param-filename 'rts "Makefile")))
      (build-makefile))

;;; pnkfelix: added the following command, to setup the runtime post
;;; calls into cleanup.sch.  The *right* way to do this would be to
;;; figure out how to encode the dependency a la make; this solution
;;; is fragile in that changes to arithmetic.mac will not be
;;; propagated without an intervening removal of arithmetic.c
  (if (not (file-exists? (param-filename 'rts "Shared" "arithmetic.c")))
      (expand-file (param-filename 'rts "Shared" "arithmetic.mac")
                   (param-filename 'rts "Shared" "arithmetic.c")))

  (let ((make-target (case *runtime-type* 
                       ((petit) 
			(case *host:os*
			  ((win32) "libpetit.lib")
			  (else    "libpetit.a")))
                       ((sparc-native) "larceny.bin")
                       ((sassy-native) 
			(case *host:os* 
			  ((win32) "larceny.bin.exe")
			  (else    "larceny.bin")))
		       )))
    ;; petit-win32.sch actually doesn't pass an arg to make... should I do same?
    (execute-in-directory (nbuild-parameter 'rts)
                          (string-append (make-command) " " make-target))))

(define build-runtime-system build-runtime)  ; Old name

(define (build-executable)
  (case *runtime-type*
    ((petit)        (build-application *petit-executable-name* '())
                    (copy-script "petit"))
    ((sparc-native sassy-native) 
     (let* ((name (case *host:os*
		    ((win32) "larceny.bin.exe")
		    (else "larceny.bin")))
	    (rts/name (param-filename 'rts name)))
       (if (file-exists? rts/name)
	   (begin
	     (copy-file/regexp (nbuild-parameter 'rts) name ".")
	     (copy-script "larceny"))
	   (error "You need to build-runtime [in order to generate Rts/larceny.bin]")
	   )))))

(define (build-development-environment)
  (case *heap-type*
    ((petit)
     (make-petit-development-environment))
    ((sparc-native)
     (make-development-environment))
    ((sassy)
     (make-development-environment)
     (make-sassy))
    (else (error 'build-development-environment "Unknown heap type"))))

(define (build-twobit)
  (build-development-environment)
  (if (eq? 'petit *runtime-type*)
      (build-application *twobit-executable-name*
                         (petit-development-environment-lop-files)))
  (copy-script "twobit"))

; Set up for loading src/Build/petit-r5rs-heap.sch
(define (build-r5rs-files)
  (case *heap-type*
    ((petit) 
     (compile-and-assemble313 (param-filename 'auxiliary "pp.sch"))
     (build-application "petit-r5rs.bin" (param-filename 'auxiliary '("pp.lop"))))
    ((sparc-native sassy)
     (compile-file (param-filename 'auxiliary "pp.sch"))
     (copy-script "larceny-r5rs"))
    (else (error 'build-r5rs-files "Unknown heap type"))))

; Set up for loading src/Build/petit-larceny-heap.sch
(define (build-larceny-files)
  (build-development-environment)
  (case *heap-type*
    ((petit)
     (build-application "petit-larceny.bin"
                        (petit-development-environment-lop-files)))
    ((sparc-native sassy)
     'done)
    (else (error 'build-larceny-files "Unknown heap type"))))
     

(define (is-macosx?)
  (string=? "MacOS X" (cdr (assq 'os-name (system-features)))))
(define load load)
(define (load-compiler . how)
  (define do-etags #f)
  (define old-load load)
  (define loaded-files '())

  (if (not (null? how))
      (case (car how)
        ((release) ;; matching code in sparc-unix.sch
         (nbuild-parameter 'always-source? #f)
         (nbuild-parameter 'verbose-load? #f)
         (nbuild-parameter 'development? #f))
        ((development) ;; matching code in petit-unix-common.sch
         (nbuild-parameter 'development? #t))
        ((etags)
         (set! do-etags #t)
         (set! load (lambda (filename)
                      (let ((val (old-load filename)))
                        (set! loaded-files (cons filename loaded-files))
                        val))))
        ))

  (cond ((eq? *target:machine* 'x86-sass)
         (compat:load-sassy)))

  (compat:load (param-filename 'util "nbuild.sch"))
  (set! load old-load)
  (cond (do-etags 
         (let ((cmd (apply string-append 
                           (cons "etags " (apply append 
                                                 (map (lambda (x) (list x " ")) loaded-files))))))
           (system cmd))))
  (if (eq? 'petit *heap-type*)
      (configure-system))
  (welcome)
  (unspecified))
  
(define (lib-suffix)
  (if (string=? (obj-suffix) ".o")
      ".a"
      ".lib"))

(define (add-lib-suffix basename)
  (string-append basename (lib-suffix)))

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
     (set! win32/petit-rts-library (param-filename 'rts
                                                   (add-lib-suffix "libpetit")))
     (set! win32/petit-lib-library (add-lib-suffix "libheap")))))

(define (remove-runtime-objects)
  (let ((libpetit.a (add-lib-suffix "libpetit"))
	(*.o        (string-append "*" (obj-suffix))))
    (delete-file/regexp (nbuild-parameter 'rts) libpetit.a)
    (delete-file/regexp (nbuild-parameter 'rts) "vc*.pdb") ; from petit-win32.sch
    (delete-file/regexp (param-filename 'rts "Sys") *.o)
    (delete-file/regexp (param-filename 'rts "Standard-C") *.o)
    (delete-file/regexp (param-filename 'rts "IAssassin") *.o)
    (delete-file/regexp (param-filename 'rts "Intel") *.o)
    (delete-file/regexp (param-filename 'rts "Build") *.o)
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
    (delete-files `("petit.bin" "petit.bin.exe"
                    ,(string-append "petit" (obj-suffix)) 
		    "petit.pdb"
		    "petit.heap" 
		    ,(add-lib-suffix "libpetit")
		    "libpetit.pdb" ; win32
		    "vc60.pdb"     ; win32
		    ,(add-lib-suffix "libheap")
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
;; (unix-&-win32-initialize)

(define (ensure-fresh-name filename suffix)
  (define filename/no-suffix 
    (substring filename 0 (- (string-length filename)
			     (string-length suffix))))
  (define (helper filename num)
    (let ((filename* (string-append filename/no-suffix 
				    "." 
				    (number->string num 16) 
				    suffix
				    )))
      (if (not (file-exists? filename*))
          filename*
          (helper filename (+ num 1)))))
  (if (not (file-exists? filename))
      filename
      (helper filename 0)))

;; ensure-slash-in-path:: String -> String
;; dlopen only works right when the path to the shared object contains
;; a slash -- otherwise it looks for system libraries.  So, we prepend
;; ./ if necessary.
(define (ensure-slash-in-path filename)
  (if (and (string=? (shared-obj-suffix) ".so")
           (not (memv #\/ (string->list filename))))
    (string-append "./" filename)
    filename))

; I think this works, but dynamic loading does not work on MacOS X 10.1.5,
; so I've been unable to test.

;; String UserData SyntaxEnv -> [Listof Segment]
(define (compile-files/file->segments infilename user syntaxenv)
  (call-with-input-file infilename 
    (lambda (in)
      (let loop ((expr (read in))
		 (segments (list)))
	(cond 
	 ((eof-object? expr) segments)
	 (else (loop (read in) 
		     (cons (assemble (compile expr syntaxenv) user)
			   segments))))))))
	 
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
	(so-name   (ensure-slash-in-path
                    (ensure-fresh-name
                     (rewrite-file-type outfilename ".fasl" (shared-obj-suffix))
		     (shared-obj-suffix)))))
    (for-each (lambda (infilename)
		(set! segments
		      (append (compile-files/file->segments infilename user syntaxenv) 
			      segments)))
	      infilenames)
    (let ((segments (reverse segments)))
      (delete-file c-name)  ; win32 doesn't do this
      (delete-file o-name)  ; or this
      (delete-file so-name) ; or this
      ;; (create-loadable-file/fasl->sharedobj outfilename segments so-name c-name o-name)
      (create-loadable-file/fasl->sharedobj outfilename segments so-name)
      (c-link-shared-object so-name 
                            (list o-name) 
                            (case *host:os*
                              ((win32) (list (param-filename 'rts (add-lib-suffix "libpetit"))))
                              (else '())))
      (unspecified))))

; eof
