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
  (lambda x (display "!! nbuild-parameter not yet set! (Util/dotnet.sch)")))

(define make-nbuild-parameter
  (lambda x (display "!! make-nbuild-parameter not yet set! (Util/dotnet.sch)")))

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
    ((win32) (load "Util\\sysdep-win32.sch"))
    ((unix macosx) (load "Util/sysdep-unix.sch"))
    (else
     (begin (display "Host = ") (display host)
            (error "unknown host!"))))

  ;; pnkfelix: Some global state is collected here.  We should probably
  ;; merge the *larceny-root* and *root-directory* global variables.
  (load (make-filename "Util" "petit-unix-defns-globals.sch"))
  
  (begin-crock 2
    (set! *larceny-root* (make-filename ""))
 
    (let ((option:source? #f)
          (option:verbose? #t)
          (option:development? #t))
      ;; set! burns my eyes!
      (set!
       make-nbuild-parameter
       (lambda (dir hostdir hostname)
	 (let ((parameters 
		`((compiler       . ,(pathname-append dir "Compiler"))
		  (util           . ,(pathname-append dir "Util"))
		  (rts            . ,(pathname-append dir "Rts"))
		  (build          . ,(pathname-append dir "Rts" "Build"))
		  (source         . ,(pathname-append dir "Lib"))
		  (common-source  . ,(pathname-append dir "Lib" "Common"))
		  (repl-source    . ,(pathname-append dir "Repl"))
		  (interp-source  . ,(pathname-append dir "Interpreter"))
		  (machine-source . ,(pathname-append dir "Lib" "IL"))
		  (mzscheme-source . ,(pathname-append dir "Lib" "MzScheme"))
		  (common-asm     . ,(pathname-append dir "Asm" "Common"))
		  (dotnet-asm     . ,(pathname-append dir "Asm" "IL"))
		  (always-source? . ,option:source?)
		  (verbose-load?  . ,option:verbose?)
		  (development?   . ,option:development?)
		  (compatibility  . ,(pathname-append dir "Compat" hostdir))
		  (auxiliary      . ,(pathname-append dir "Auxlib"))
		  (root           . ,dir)
		  (host-system    . ,hostname)
		  (target-machine . dotnet)
		  (target-os      . ,option:os)
		  (host-os        . ,option:os)
		  (endianness     . ,option:endian)
		  (target-endianness . ,option:endian)
		  (host-endianness . ,option:endian)
		  (word-size      . 32)
		  )))
	   (lambda (key . rest)
             (let ((probe (assq key parameters)))
               (if (null? rest)
                   (if probe 
                       (cdr probe)
                       #f)
                   (let ((new-val (car rest)))
                     (set! parameters (cons (cons key new-val) parameters))))
               ))))))
     
  
  ;; set this so everybody can use it
  (set! nbuild-parameter
        (make-nbuild-parameter *larceny-root* host host)))

  ;; Load the compatibility file, expander, and config system.
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (load (string-append (nbuild-parameter 'util) "csharp-config.scm"))
  )


(define (setup-directory-structure)
  (case option:os
    ((unix macosx) (system "mkdir Rts/Build"))
    ((win32) (system "mkdir Rts\\Build"))))

(define (build-config-files)
  ;; Generate the C# code for the constant definitions.
  (define run-csharp-config
    (let ((output-c#-file
           (make-filename *larceny-root* "Rts" "DotNet" "Constants.cs"))
          (rts-dir (make-filename *larceny-root* "Rts")))
      (lambda ()
        (csharp-config 
         output-c#-file
         `((,(make-filename rts-dir "layouts.cfg") int)
           (,(make-filename rts-dir "except.cfg")  int)
           (,(make-filename rts-dir "globals.cfg") int)
           (,(make-filename rts-dir "mprocs.cfg")  int))))))
      
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

  (display " -- Copying config files to Build directory")(newline)
  (for-each
   (lambda (cfgfile)
     (let ((src-file (make-filename *larceny-root* "Rts" cfgfile))
           (target-file
            (make-filename *larceny-root* "Rts" "Build" cfgfile)))
       (if (not (file-exists? target-file))
           (catfiles (list src-file) target-file))))
   (map (lambda (f) (make-filename (string-append f ".cfg")))
        cfg-names))
  
  ;; we don't need the C code
  ;;(expand-file (build-path "Standard-C" "arithmetic.mac")
  ;;             (build-path "Standard-C" "arithmetic.c"))
  
  ;(parameterize [(current-directory *root-directory*)]
  (display " -- Running config ...")(newline)
  (for-each config
            (map (lambda (f) (make-filename *larceny-root*
                                            "Rts"
                                            (string-append f ".cfg")))
                 cfg-names))
  (if (not (file-exists? (make-filename *larceny-root* "Rts" "Build" "cdefs.h")))
      (begin
        (catfiles
         (map (lambda (f) (make-filename *larceny-root*
                                         "Rts"
                                         "Build"
                                         (string-append f ".ch")))
              cfg-names)
         (make-filename *larceny-root* "Rts" "Build" "cdefs.h"))))

  (if (not (file-exists? (make-filename *larceny-root* "Rts" "Build" "schdefs.h")))
      (begin
        (catfiles
         (map (lambda (f) (make-filename *larceny-root*
                                         "Rts"
                                         "Build"
                                         (string-append f ".sh")))
              (remove "mprocs" cfg-names))
         (make-filename *larceny-root* "Rts" "Build" "schdefs.h"))))
  
  (display " -- Running C# config...")(newline)
  (let ((file (make-filename *larceny-root* "Rts" "DotNet" "Constants.cs")))
    (if (file-exists? file)
        (delete-file file)))
  (run-csharp-config))

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
  (load (make-filename *larceny-root* "Util" "nbuild.sch"))
  (for-each set-codegen-option! option:codegen-options))

(define (build-runtime-system)
  (let ((cmd-string 
	 (twobit-format 
	  #f (case (nbuild-parameter 'host-os)
	       ((win32)       "cd Rts\\DotNet && nmake.exe ~a DEBUG_OPT=\"~a\" DEFINES=\"~a\"")
	       ((unix macosx) "cd Rts/DotNet;    make      ~a DEBUG_OPT='~a'   DEFINES='~a'")
	       (else
		(error "Unknown operating system: " (nbuild-parameter 'host-os))))
	  (if (codegen-option 'mono)
	      "CSC=mcs "
	      "CSC=csc ")
	  (if (codegen-option 'debug)
	      "/checked+ /warn:4 /debug:full /d:DEBUG "
	      "/optimize+ ")
	  (string-append 
	   (if (eq? (nbuild-parameter 'endianness) 'big) 
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
  (define crock-file-1 "Rts/Build/dotnet-twobit-1.sch")
  (define crock-file-2 "Rts/Build/dotnet-twobit-2.sch")  

  (cond ((file-exists? "Twobit.fasl")
	 (delete-file "Twobit.fasl")))
  
  (write-crock-one crock-file-1)
  (write-crock-two crock-file-2)

  (compile-application 
   app-name
   (append (list "Util/dotnet.sch") 
	   ;; Next bunch is the result of breaking down larceny-setup
	   ;; into seperate components seperated by its calls to load
	   (list crock-file-1)
	   (case option:os 
	     ((win32) (list "Util\\sysdep-win32.sch"))
	     ((unix macosx) (list "Util/sysdep-unix.sch")))
	   (list "Util/petit-unix-defns-globals.sch")
	   (list crock-file-2)
	   (list "Compat/Larceny/compat.sch" "Compat/Larceny/compat2.sch"
		 "Auxlib/list.sch" "Auxlib/pp.sch")
	   (list "Util/expander.sch" "Util/config.sch"
		 "Util/csharp-config.scm")
	   ;; Rest is from load-compiler
	   (list "Util/nbuild-files.sch" "Util/nbuild-defns.sch"
		 ;; "Util/nbuild.sch" ;; This does the loading that's inlined below
		 )
	   (nbuild:twobit-files)
	   (nbuild:common-asm-files)
	   (nbuild:machine-asm-files)
	   (nbuild:utility-files)
	   (list "Rts/make-templates.sch" "Util/cleanup.sch")

           additional-files
	   )))

(define (build-twobit)
  (build-twobit-base "Twobit" 
                     '("Util/dotnet-compile-file.sch")))

(define (build-larceny)
  (build-twobit-base "Larceny" 
                     '("Compiler/driver-larceny.sch"
                       "Util/seal-twobit.sch"
                       "Util/dotnet-compile-file.sch"
                       
                       "Asm/Common/link-lop.sch"
                       "Asm/IL/il-jdot-aliases.sch"
                       "Asm/IL/il-corememory.sch"

                       "Debugger/debug.sch"
                       "Debugger/inspect-cont.sch"
                       "Debugger/trace.sch"
                       
                       "Util/dotnet-larceny.sch")))

;; Convenience
(define (load-debugger)
  (load (make-filename *larceny-root* "Debugger" "trace.sch"))
  (load (make-filename *larceny-root* "Debugger" "inspect-cont.sch"))
  (load (make-filename *larceny-root* "Debugger" "debug.sch"))
  (install-debugger))
