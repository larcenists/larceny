;; Provides a procedure
;; larceny-setup : host-sym OS-sym endianness codegen-option ... -> ???
;;
;; Loads appropriate system-dependent stuff

;; BEFORE LOADING THIS FILE:  Make sure your Scheme interpreter's
;; current-directory is the root of this source tree.

;; TODO:  Umm... finish.  Also, fix nbuild.sch and nbuild-files.sch
;;   Gotta separate the new compiler sources from the old Std. C
;;   and add them to the larceny_src

;; larceny root should be the current directory when the host
;; Scheme system loads this file.
(define *larceny-root* #f)

;; this needs to be global... it floats all around the build-system.
;; it will be set to something meaningful by (larceny-setup ...).
(define nbuild-parameter
  (lambda x (display "!! nbuild-parameter not yet set! (src/Build/dotnet.sch)")))

(define make-nbuild-parameter
  (lambda x (display "!! make-nbuild-parameter not yet set! (src/Build/dotnet.sch)")))

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

;; These needs to be global for the definition of lib-files
;; They are set!'d by larceny-setup
(define option:os #f)
(define option:endian #f)
(define option:codegen-options '())

(define system-big-endian?
  (lambda x (display "!! system-big-endian not set yet")(newline)))

(define copy-file
  (lambda x (display "!! copy-file not set yet") (newline)))

(define body-crock-1 '())
(define body-crock-2 '())

(define-syntax begin-crock 
  (syntax-rules ()
    ((_ 1)
     (if #f #f))
    ((_ 2)
     (if #f #f))
    ((_ 1 EXP REST ...)
     (begin (set! body-crock-1 (append body-crock-1 (list (quote EXP))))
	    EXP
	    (begin-crock 1 REST ...)))
    ((_ 2 EXP REST ...)
     (begin (set! body-crock-2 (append body-crock-2 (list (quote EXP))))
	    EXP
	    (begin-crock 2 REST ...)))))

;; FIXME:  figure out endian from host scheme system?
(define (larceny-setup host os endian . codegen-options)
  (begin-crock 1  ;; see crock craziness above and below
   (set! option:os os)
   (set! option:endian endian)
   (set! option:codegen-options codegen-options)

   (case option:endian
     ((big) (set! system-big-endian? (lambda () #t)))
     ((little) (set! system-big-endian? (lambda () #f)))))
  
  ;; FIXME:  might have to fudge more this for Cygwin
  ;; load code to work with pathnames
  (case option:os
    ((win32) (load "src/Build/sysdep-win32.sch"))
    ((unix macosx) (load "src/Build/sysdep-unix.sch"))
    (else
     (begin (display "Host = ") (display host)
            (error "unknown host!"))))

  ;; pnkfelix: Some global state is collected here.  We should probably
  ;; merge the *larceny-root* and *root-directory* global variables.
  (load (make-filename "src" "Build" "petit-unix-defns-globals.sch"))
  
  (begin-crock 2
    (set! *larceny-root* (make-filename ""))

    (load (make-filename "src" "Build" "nbuild-param.sch"))

    ;; set this so everybody can use it
    (set! nbuild-parameter
      (let ((dir *larceny-root*)
            (option:source? #f)
            (option:verbose? #t)
            (option:development? #t))
        (make-nbuild-parameter
          'root                  dir
          'machine-source        (pathname-append dir "src" "Lib" "IL")
          'mzscheme-source       (pathname-append dir "lib" "MzScheme")
          'dotnet-asm            (pathname-append dir "src" "Asm" "IL")
          'always-source?        option:source?
          'verbose-load?         option:verbose?
          'development?          option:development?
          'compatibility         (pathname-append dir "src" "Compat" host)
          'host-system           host
          'target-machine        'dotnet
          'target-os             option:os
          'host-os               option:os
          ; We should be careful to use host-endianness or
          ; target-endianness, because "endianness" alone doesn't mean
          ; anything.
          ; 'endianness            option:endian
          'target-endianness     option:endian
          'host-endianness       option:endian
          'word-size             32)))
      )

  ;; Load the compatibility file, expander, and config system.
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (load (string-append (nbuild-parameter 'util) "csharp-config.sch"))
  )


(define (build-config-files)
  (define (catfiles input-files output-file)
    (with-output-to-file output-file
      (lambda ()
        (for-each
         (lambda (infile)
           (with-input-from-file infile
             (lambda ()
               (let loop ()
                 (let ((next (read-char)))
                   (if (not (eof-object? next))
                       (begin (write-char next)
                              (loop))))))))
         input-files))))

  (define cfg-names '("except" "globals" "layouts" "mprocs"))

  (for-each (lambda (f)
              (config (param-filename 'rts (string-append f ".cfg"))))
            cfg-names)

  (let ((cdefs (param-filename 'include "cdefs.h")))
    (if (not (file-exists? cdefs))
        (catfiles
          (map (lambda (f)
                 (param-filename 'include (string-append f ".ch")))
               cfg-names)
          cdefs)))

  (let ((file (param-filename 'rts "DotNet" "Constants.cs")))
    (if (file-exists? file)
        (delete-file file))
    (csharp-config
     file
     (map (lambda (f)
            (list (param-filename 'rts (string-append f ".cfg"))
                  'int))
          cfg-names))))

;; Load the compiler
(define (load-compiler . how)
  (if (not (null? how))
      (case (car how)
        ((release)
         (nbuild-parameter 'always-source? #f)
         (nbuild-parameter 'verbose-load? #f)
         (nbuild-parameter 'development? #f))
        ((development)
         (nbuild-parameter 'development? #t))))
  (load (param-filename 'util "nbuild.sch"))
  (for-each set-codegen-option! option:codegen-options))

(define (build-runtime-system)
  (let ((cmd-string 
	 (twobit-format 
	  #f (case (nbuild-parameter 'host-os)
	       ((win32)       "cd src\\Rts\\DotNet && nmake.exe ~a DEBUG_OPT=\"~a\" DEFINES=\"~a\"")
	       ((unix macosx) "cd src/Rts/DotNet;    make      ~a DEBUG_OPT='~a'   DEFINES='~a'")
	       (else
		(error "Unknown operating system: " (nbuild-parameter 'host-os))))
	  (if (codegen-option 'mono)
	      "CSC=mcs "
	      "CSC=csc ")
	  (if (codegen-option 'debug)
	      "/checked+ /warn:4 /debug:full /d:DEBUG "
	      "/optimize+ ")
	  (string-append 
	   (if (eq? (nbuild-parameter 'target-endianness) 'big) 
	       "/d:BIG_ENDIAN " "")
	   (if (codegen-option 'mono) 
	       "/d:USING_MONO " "")
	   (if (codegen-option 'rotor)
	       "/d:USING_ROTOR " "")
	   (if (and (not (codegen-option 'mono)) 
		    (eq? (nbuild-parameter 'host-os) 'win32))
	       "/d:HAS_OSVERSION /d:HAS_PERFORMANCE_COUNTERS /d:HAS_WINDOWS_FORMS " "")
	   (if (memq (nbuild-parameter 'host-os) '(unix macosx))
	       "/d:USING_UNIX " "")
	   (if (codegen-option 'clr-2.0)
	       "/d:HAS_SETENV_SUPPORT " "")
	   ))))
    (display cmd-string) (newline)
    (system cmd-string)))

(define (write-crock num port body)
  (define (displayln x) (display x port) (newline port))
  (define (writeln x) (write x port) (newline port))
  (displayln (string-append 
	      ";; This (autogenerated) file is part " (number->string num) 
	      " of a two-part crock."))
  (displayln  ";; Felix essentially took larceny-setup from dotnet.sch,      ")
  (displayln  ";; split it into component pieces, interleaving the loading   ")
  (displayln  ";; and global variable initialization as necessary, in order  ")
  (displayln  ";; to create a parallel setup to that of larceny-setup itself.")
  (displayln  ";; (larceny-setup \"Larceny\" os  endian)                     ")
  (writeln `(let ((host   "Larceny")
		  (os     ',option:os)
		  (endian ',option:endian)
		  (codegen-options ',option:codegen-options))
	      ,@body)))

(define (write-crock-one file)
  (if (file-exists? file)
      (delete-file file))
  (let ((p (open-output-file file)))
    (write-crock 1 p body-crock-1)
    (close-output-port p)))

(define (write-crock-two file)
  (if (file-exists? file)
      (delete-file file))
  (let ((p (open-output-file file)))
    (write-crock 2 p body-crock-2)
    (close-output-port p)))
  
(define (build-twobit-base app-name additional-files)
  (define crock-file-1 (param-filename 'util "dotnet-twobit-1.sch"))
  (define crock-file-2 (param-filename 'util "dotnet-twobit-2.sch"))

  (write-crock-one crock-file-1)
  (write-crock-two crock-file-2)

  (compile-application 
   app-name
   (append (param-filename 'util '("dotnet.sch")) 
	   ;; Next bunch is the result of breaking down larceny-setup
	   ;; into seperate components seperated by its calls to load
	   (list crock-file-1)
           (param-filename 'util
                           (case option:os 
                             ((win32) '("sysdep-win32.sch"))
                             ((unix macosx) '("sysdep-unix.sch"))))
	   (param-filename 'util '("petit-unix-defns-globals.sch"))
	   (list crock-file-2)
           (param-filename 'larceny-compatibility '("compat.sch" "compat2.sch"))
           (param-filename 'auxiliary '("list.sch" "pp.sch"))
           (param-filename 'util '("expander.sch" "config.sch"
                                   "csharp-config.sch"))
	   ;; Rest is from load-compiler
           (param-filename 'util '("nbuild-files.sch" "nbuild-defns.sch"))
		 ;; "Util/nbuild.sch" ;; This does the loading that's inlined below
	   (nbuild:twobit-files)
	   (nbuild:common-asm-files)
	   (nbuild:machine-asm-files)
	   (nbuild:utility-files)
           (param-filename 'rts '("make-templates.sch"))
           (param-filename 'util '("cleanup.sch"))

           additional-files
	   )))

(define (build-twobit)
  (cond ((file-exists? "Twobit.fasl")
	 (delete-file "Twobit.fasl")))
  (build-twobit-base "Twobit" 
                     (param-filename 'auxiliary "dotnet-compile-file.sch")))

(define (build-larceny)
  (cond ((file-exists? "Larceny.fasl")
	 (delete-file "Larceny.fasl")))
  (build-twobit-base "Larceny"
                     `(,@(param-filename 'compiler '("driver-larceny.sch"))
                       ,@(param-filename 'util '("seal-twobit.sch"))
                       ,@(param-filename 'auxiliary '("dotnet-compile-file.sch"))
                       ,@(param-filename 'common-asm '("link-lop.sch"))
                       ,@(param-filename 'dotnet-asm '("il-jdot-aliases.sch"
                                                       "il-corememory.sch"))
                       ,@(param-filename 'debugger '("debug.sch"
                                                     "inspect-cont.sch"
                                                     "trace.sch"))
                       ,@(param-filename 'util '("dotnet-larceny.sch")))))

;; Convenience
;(define (load-debugger)
; (for-each load (param-filename 'debugger '("trace.sch"
;                                            "inspect-cont.sch"
;                                            "debug.sch")))
; (install-debugger))


