;; Provides a procedure
;; larceny-setup : host-sym OS-sym -> ???
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

;; this needs to be global for the definition of lib-files
(define option:os #f)

(define system-big-endian?
  (lambda x (display "!! system-big-endian not set yet")(newline)))

(define copy-file
  (lambda x (display "!! copy-file not set yet") (newline)))

;; FIXME:  figure out endian from host scheme system?
(define (larceny-setup host os option:endian)
  (set! option:os os)

  (case option:endian
    ((big) (set! system-big-endian? (lambda () #t)))
    ((little) (set! system-big-endian? (lambda () #f))))
  
  ;; FIXME:  might have to fudge more this for Cygwin
  ;; load code to work with pathnames
  (case option:os
    ((win32) (load "Util\\sysdep-win32.sch"))
    ((unix macosx) (load "Util/sysdep-unix.sch"))
    (else
     (begin (display "Host = ") (display host)
            (error "unknown host!"))))

  (set! *larceny-root* (make-filename ""))

  
  ;; Standard-C version.  Have to fix this once .Net backend
  ;; stops needing to load & mutate the Standard-C backend.  It might also
  ;; be nice if a bunch of the options weren't hardcoded.
  (let ((option:source? #t)
        (option:verbose? #f)
        (option:development? #t))
    ;; set! burns my eyes!
    (set!
     make-nbuild-parameter
     (lambda (dir hostdir hostname)
       (let ((parameters 
              `((compiler       . ,(pathname-append dir "Twobit"))
                (util           . ,(pathname-append dir "Util"))
                (build          . ,(pathname-append dir "Rts" "Build"))
                (source         . ,(pathname-append dir "Lib"))
                (common-source  . ,(pathname-append dir "Lib" "Common"))
                (repl-source    . ,(pathname-append dir "Repl"))
                (interp-source  . ,(pathname-append dir "Interpreter"))
                (machine-source . ,(pathname-append dir "Lib" "IL"))
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
         (lambda (key)
           (let ((probe (assq key parameters)))
             (if probe 
                 (cdr probe)
                 #f)))))))
     
  
  ;; set this so everybody can use it
  (set! nbuild-parameter
        (make-nbuild-parameter *larceny-root* host host))

  ;; Load the compatibility file, expander, and config system.
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (set! config-path "Rts/Build/")
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
  (run-csharp-config))


;; Load the compiler
(define (load-compiler)
  (load (make-filename *larceny-root* "Util" "nbuild.sch")))

(define (build-runtime-system)
  (case (nbuild-parameter 'host-os)
    ((win32)
     (system "cd Rts\\DotNet && nmake"))
    ((unix macosx)
     (system "cd Rts/DotNet; make"))
    (else
     (error "Unknown operating system: " (nbuild-paramter 'host-os)))))


;; Convenience
(define (load-debugger)
  (load (make-filename *larceny-root* "Debugger" "trace.sch"))
  (load (make-filename *larceny-root* "Debugger" "inspect-cont.sch"))
  (load (make-filename *larceny-root* "Debugger" "debug.sch"))
  (install-debugger))
