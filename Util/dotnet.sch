;; Provides a procedure
;; larceny-setup : host-sym OS-sym -> ???
;;
;; Loads appropriate system-dependent stuff

;; TODO:  Umm... finish.  Also, fix nbuild.sch and nbuild-files.sch
;;   Gotta separate the new compiler sources from the old Std. C
;;   and add them to the larceny_src


;; FIXME:  figure out endian from host scheme system?
(define (larceny-setup host option:os option:endian)
  ;; FIXME:  might have to fudge more this for Cygwin
  ;; load code to work with pathnames
  (case option:os
    ((win32) (load "sysdep-win32.sch"))
    ((unix macosx) (load "sysdep-unix.sch"))
    (else
     (begin (display "Host = ") (display host)
            (error "unknown host!"))))

  ;; larceny root should be one up from the current directory
  (define *larceny-root*
    (pathname-append (make-filename "" "..")))

  (define *expander-file*
    (make-filename *larceny-root* "Util" "expander.sch"))

  (define *config-file*
    (make-filename *larceny-root* "Util" "config.sch"))

  
  (define option:source? #t)
  (define option:verbose? #f)
  (define option:development? #t)

  (define nbuild-param
    (make-nbuild-parameter *larceny-root* host host))

  ;; Load the compatibility file, expander, and config system.
  (load (string-append (nbuild-parameter 'compatibility) "compat.sch"))
  (compat:initialize)
  (load (string-append (nbuild-parameter 'util) "expander.sch"))
  (load (string-append (nbuild-parameter 'util) "config.sch"))
  (set! config-path "Rts/Build/")

  )

(define (setup-directory-structure)
  (make-directory* (build-path *root-directory* "Rts" "Build")))

(define (run-csharp-config)
  (csharp-config 
   (build-path LARCENY-PATH "runtime" "Constants.cs")
   `((,(build-path *root-directory* "Rts" "layouts.cfg")  int)
     (,(build-path *root-directory* "Rts" "except.cfg")  uint)
     (,(build-path *root-directory* "Rts" "globals.cfg") uint)
     (,(build-path *root-directory* "Rts" "mprocs.cfg")  uint))))


(define (make-nbuild-parameter dir hostdir hostname)
  (let ((parameters 
         `((compiler       . ,(pathname-append dir "Twobit"))
           (util           . ,(pathname-append dir "Util"))
           (build          . ,(pathname-append dir "Rts" "Build"))
           (source         . ,(pathname-append dir "Lib"))
           (common-source  . ,(pathname-append dir "Lib" "Common"))
           (repl-source    . ,(pathname-append dir "Repl"))
           (interp-source  . ,(pathname-append dir "Interpreter"))
           ;; FIXME:  figure out what Lib/Standard-C is for
           (machine-source . ,(pathname-append dir "Lib" "Standard-C"))
           (common-asm     . ,(pathname-append dir "Asm" "Common"))
           (il-asm         . ,(pathname-append dir "Asm" "IL"))
           (always-source? . ,option:source?)
           (verbose-load?  . ,option:verbose?)
	   (development?   . ,option:development?)
           (compatibility  . ,(pathname-append dir "Compat" hostdir))
           (auxiliary      . ,(pathname-append dir "Auxlib"))
           (root           . ,dir)
           (host-system    . ,hostname)
           ;; FIXME:  hardcoded dotNet
           (target-machine . dotNet)
           (target-os      . ,option:os)
           (host-os        . ,option:os)
           (endianness     . ,option:endian)
           (target-endianness . ,option:endian)
           (host-endianness . ,option:endian)
           ;; FIXME:  hardcoded word size
           (word-size      . 32)
           )))
    (lambda (key)
      (let ((probe (assq key parameters)))
        (if probe 
            (cdr probe)
            #f)))))