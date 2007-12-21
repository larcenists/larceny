; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny's ERR5RS and R6RS modes.
; Code names:
;     Aeryn    ERR5RS
;     D'Argo   R6RS-compatible
;     Spanky   R6RS-conforming (not yet implemented)

(define (aeryn-evaluator exp . rest)
  (ex:repl (list exp)))

; FIXME:  This probably isn't right for Common Larceny.

(define aeryn-fasl-evaluator
  (let* ((arch-name (cdr (assq 'arch-name (system-features))))
         (fasl-value
          (cond ((string=? arch-name "Standard-C")
                 '(import (rnrs base)
                          (primitives .petit-shared-object
                                      .petit-patch-procedure)))
                ((string=? arch-name "CLR")
                 (unspecified))
                (else
                 (unspecified)))))
    (lambda () fasl-value)))


; FIXME:  This is for compiling by hand.  It should be moved into
; test/Scripts/package-bin-release.sh

(define (larceny:compile-r6rs-runtime)
  (parameterize ((current-directory (current-larceny-root)))
    (define (compile-r6rs-runtime-core)
      (parameterize ((current-directory "lib/R6RS"))
        (compile-file "r6rsmode.sch")
        (compile-file "r6rs-compat-larceny.sch")
        (compile-file "r6rs-runtime.sch")
        (compile-file "r6rs-expander.sch")
        (require 'r6rs-compat-larceny)
        (require 'r6rs-runtime)
        (require 'r6rs-expander)
        (ex:expand-file "r6rs-standard-libraries.sch"
                        "r6rs-standard-libraries.exp")
        (compile-file "r6rs-standard-libraries.exp"
                      "r6rs-standard-libraries.fasl")
        (require 'r6rs-standard-libraries)))
    (define (compile-source-libraries)
      (parameterize ((compile-libraries-older-than-this-file
                      (string-append
                       (current-directory)
                       "/lib/R6RS/r6rs-standard-libraries.fasl")))
        (for-each larceny:compile-libraries
                  (current-require-path))))
    (time (compile-r6rs-runtime-core))
    (time (compile-source-libraries))))

; Given the absolute pathname for a reference file in some directory,
; compiles all ERR5RS/R6RS library files that directory and its
; subdirectories that are older than the reference file.
;
; The reference file is typically a file that has been modified,
; so all files that depend upon it must be recompiled anyway.
;
; FIXME: It might be better to compile only the files that depend
; upon the reference file, but that's a little harder.

(define (compile-stale-libraries . rest)
  (cond ((null? rest)
         (let ((fname (generate-temporary-name
                       (string-append (current-directory) "/temporary"))))
           (call-with-output-file fname values)
           (dynamic-wind
            (lambda () #t)
            (lambda () (compile-stale-libraries fname))
            (lambda () (delete-file fname)))))
        ((and (null? (cdr rest))
              (string? (car rest)))
         (let ((fname (car rest)))
           (larceny:load-r6rs-package)
           (parameterize ((compile-libraries-older-than-this-file fname))
             (larceny:compile-libraries
              (larceny:directory-of fname)))))
        (else
         (assertion-violation 'compile-stale-libraries
                              "illegal arguments"
                              rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Directory hacking.
;
; FIXME:  This stuff really doesn't belong here, but
; lib/Standard/file-system.sch currently relies on the FFI,
; which relies on having a C compiler properly installed and
; the execution path set correctly.
;
; FIXME:  Many of these procedures write temporary files in
; some directory.  That works because those directories should
; be ignored anyway if we don't have write permission in them,
; but it isn't the right way to do this stuff.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns one of these symbols: unix, windows, unknown.

(define larceny:os
  (let ((os #f))
    (lambda ()
      (or os
          (let ((os-name (cdr (assq 'os-name (system-features)))))
            (cond ((member os-name '("SunOS" "Linux" "MacOS X"))
                   (set! os 'unix))
                  ((member os-name '("Win32"))
                   (set! os 'windows))
                  (else
                   (set! os 'unknown)))
            os)))))

(define (larceny:unsupported-os)
  (error 'larceny:compile-r6rs-runtime
         "can't find library files for this OS yet"))

(define (larceny:system-transcoder)
  (case (larceny:os)
   ((unix)
    (native-transcoder))
   ((windows)
    (make-transcoder (latin-1-codec) ; [sic]
                     'crlf))
   (else
    (native-transcoder))))

(define (larceny:separator)
  (case (larceny:os)
   ((windows) "\\")
   (else "/")))

; Converts Windows pathnames to a canonical form
; by replacing forward slashes with backslashes
; and by ignoring all but the first of each sequence
; of consecutive slashes.

(define (larceny:canonical-path path)
  (define (loop rchars chars ignore-slash)
    (cond ((null? rchars)
           (list->string chars))
          ((memv (car rchars) '(#\\ #\/))
           (loop (cdr rchars)
                 (if ignore-slash chars (cons #\\ chars))
                 #t))
          (else
           (loop (cdr rchars) (cons (car rchars) chars) #f))))
  (loop (reverse (string->list path)) '() #f))

(define (larceny:list-directory path)
  (if (not (larceny:directory? path))
      '()
      (let* ((tempfile
              (generate-temporary-name
               (string-append (current-directory)
                              (larceny:separator)
                              "temporary")))
             (result
              (case (larceny:os)
               ((unix)
                (system
                 (string-append "ls -1 " path " > " tempfile)))
               ((windows)
                (system
                 (string-append "dir /B \"" path "\" > \"" tempfile "\"")))
               (else 1)))
             (files
              (if (zero? result)
                  (call-with-port
                   (transcoded-port (open-file-input-port tempfile)
                                    (larceny:system-transcoder))
                   (lambda (p)
                     (do ((file (get-line p) (get-line p))
                          (files '() (cons file files)))
                         ((eof-object? file)
                          (reverse files)))))
                  '())))
        (delete-file tempfile)
        files)))

(define (larceny:list-subdirectories path)
  (case (larceny:os)
   ((unix)
    (filter larceny:directory? (larceny:list-directory path)))
   ((windows)
    (let* ((tempfile
            (larceny:canonical-path
             (generate-temporary-name
              (string-append (current-directory)
                             (larceny:separator)
                             "temporary"))))
           (result
            (system
             (string-append
              "dir /AD /B \""
              (larceny:canonical-path path)
              "\" > \"" tempfile "\"")))
           (directories
            (if (zero? result)
                (call-with-port
                 (transcoded-port (open-file-input-port tempfile)
                                  (larceny:system-transcoder))
                 (lambda (p)
                   (do ((file (get-line p) (get-line p))
                        (files '() (cons file files)))
                       ((eof-object? file)
                        (reverse files)))))
                '())))
      (delete-file tempfile)
      directories))
   (else '())))

(define (larceny:directory? path)
  (case (larceny:os)
   ((unix)
    (zero? (system (string-append "ls " path "/* 2>/dev/null >/dev/null"))))
   ((windows)
    (zero? (system (string-append
                    "dir /AD /B \"" (larceny:canonical-path path) "\""))))
   (else
    (larceny:unsupported-os))))

; Under Windows, converts to a canonical form that
; replaces backward slashes by forward slashes
; and ignores all but the first of a sequence of consecutive slashes.
; FIXME: doesn't handle . or ..

(define (larceny:directory-of fname)
  (case (larceny:os)
   ((unix)
    (if (absolute-path-string? fname)
        (do ((i (- (string-length fname) 1) (- i 1)))
            ((char=? (string-ref fname i) #\/)
             (substring fname 0 i)))
        (larceny:directory-of (string-append (current-directory) "/" fname))))
   ((windows)
    (if (absolute-path-string? fname)
        (do ((i (- (string-length fname) 1) (- i 1)))
            ((or (< i 0)
                 (memv (string-ref fname i)'(#\\ #\/)))
             (larceny:canonical-path (substring fname 0 (max i 0)))))
        (larceny:directory-of
         (string-append (current-directory) "\\" fname))))
   (else
    (larceny:unsupported-os))))    

(define (larceny:compile-libraries path)
  (let ((basefile (compile-libraries-older-than-this-file)))
    (parameterize ((fasl-evaluator aeryn-fasl-evaluator)
                   (load-evaluator aeryn-evaluator)
                   (repl-evaluator aeryn-evaluator))
      (case (larceny:os)
       ((unix windows)
        (let ()
          (define (compile-libraries path)
            (if (larceny:directory? path)
                (let ((files (larceny:list-directory path)))
                  (parameterize ((current-directory path))
                    (for-each (lambda (file)
                                (if (larceny:directory? file)
                                    (compile-libraries file)))
                              files)
                    (for-each (lambda (file)
                                (if (and (file-type=? file ".sls")
                                         (let ((slfasl
                                                (rewrite-file-type file
                                                                   ".sls"
                                                                   ".slfasl")))
                                           (or (not (file-exists? slfasl))
                                               (file-newer? basefile
                                                            slfasl))))
                                    (compile-r6rs-file file #f #t)))
                              files)))))
          (compile-libraries path)))
       (else
        (larceny:unsupported-os))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (larceny:load-r6rs-runtime)
  (require 'r6rs-compat-larceny)
  (require 'r6rs-runtime)
  (require 'r6rs-standard-libraries))

(define (larceny:load-r6rs-package)
  (require 'r6rs-compat-larceny)
  (require 'r6rs-runtime)
  (require 'r6rs-expander)
  (require 'r6rs-standard-libraries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Called by interactive-entry-point in Lib/Repl/main.sch

(define (run-r6rs-forms forms)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-sequence forms))

; Called by interactive-entry-point in Lib/Repl/main.sch
;
; Fasl files must be raw Latin-1, while source files may be
; UTF-8 or UTF-16 on some platforms.
; To detect fasl files, we open the file as raw Latin-1 and
; look at the first line.  If it's a fasl file, we load it
; as a fasl file.  Otherwise we close the raw Latin-1 port
; and load the file as a source file.

(define (run-r6rs-program filename)
  (larceny:load-r6rs-package)
  (if (call-with-port
       (open-raw-latin-1-input-file filename)
       (lambda (p)
         (let ((first-line (get-line p)))
           (cond ((and (string? first-line)
                       (string=? first-line "#!fasl"))
                  (aeryn-evaluator (aeryn-fasl-evaluator)
                                   interaction-environment)
                  (load-from-port p interaction-environment)
                  #t)
                 (else
                  #f)))))
      (unspecified)
      (ex:run-r6rs-program filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Auto-loading and auto-compiling of ERR5RS/R6RS libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; When (rnrs base) is recompiled, then all of the older .slfasl
; files need to be recompiled.
; FIXME:  Should limit to files that depend on the recompiled file(s).

(define compile-libraries-older-than-this-file
  (make-parameter "compile-libraries-older-than-this-file" #f))

; Never load a library file twice.
;
; We can't rely on require to do this for us, because
; we'd have to parameterize (current-require-path) for
; each call to require, and then recursive calls to
; larceny:autoload-r6rs-library would be executed with
; the wrong (current-require-path).
;
; FIXME:  Do we need to detect circular dependencies?

(define larceny:autoloaded-r6rs-library-files '())

; Called by ex:lookup-library in lib/R6RS/r6rs-runtime.sch
; Returns false is the library has previously been loaded,
; returns true if the file exists and loads successfully,
; or raises an exception.

(define (larceny:autoload-r6rs-library libname)

  ; FIXME: this isn't the best place for pathname hacking

  (define (make-path path)
    (apply string-append
           (cons (car path)
                 (map (lambda (s) (string-append (larceny:separator) s))
                      (cdr path)))))

  ; Returns true iff the second file is in the same directory
  ; as the first, or in a subdirectory of that directory.

  (define (in-same-directory? fname1 fname2)
    (string-prefix? (larceny:directory-of fname1)
                    (larceny:directory-of fname2)))

  (define (string-prefix? s1 s2)
    (let ((n1 (string-length s1))
          (n2 (string-length s2)))
      (and (<= n1 n2)
           (string=? s1 (substring s2 0 n1)))))

  (define (register! fname)
    (set! larceny:autoloaded-r6rs-library-files
          (cons fname larceny:autoloaded-r6rs-library-files))
   ;(display "    from ")
   ;(display fname)
   ;(newline)
    #t)

  (define (load-r6rs-library fname)
    (cond ((member fname larceny:autoloaded-r6rs-library-files)
           #f)
          ((compile-libraries-older-than-this-file)
           =>
           (lambda (reference-file)
             (cond ((file-type=? fname ".sls")
                    (compile-r6rs-file fname #f #t)
                    (let ((fname (rewrite-file-type fname ".sls" ".slfasl")))
                      (register! fname)
                      (load fname)
                      #t))
                   ((file-type=? fname ".slfasl")
                    (let ((src (rewrite-file-type fname ".slfasl" ".sls")))
                      (if (and (file-exists? src)
                               (or (file-newer? src fname)
                                   (and
                                    (in-same-directory? reference-file fname)
                                    (file-newer? reference-file fname))))
                          (begin (compile-r6rs-file src fname #t)
                                 (register! fname)
                                 (load fname)
                                 #t)
                          (begin (register! fname)
                                 (load fname)
                                 #t))))
                   (else
                    (register! fname)
                    (load fname)
                    #t))))
          (else
           (register! fname)
           (load fname)
           #t)))

  (display "Autoloading ")
  (write libname)
  (newline)

  (let* ((libpath (map symbol->string libname))
         (libpaths (do ((libpath (reverse libpath) (cdr libpath))
                        (libpaths '() (cons libpath libpaths)))
                       ((null? libpath)
                        (reverse libpaths))))
         (require-paths (current-require-path))
         (fname
          (call-with-current-continuation
           (lambda (return)
             (for-each
              (lambda (rpath)
                (for-each (lambda (lpath)
                            (let ((path (make-path
                                         (cons rpath (reverse (cdr lpath)))))
                                  (name (car lpath)))
                              '
                              (begin (display "Trying ")
                                     (display path)
                                     (display "/")
                                     (display name)
                                     (newline))
                              (parameterize
                               ((current-require-path (list path))
                                (current-require-path-suffix-optional #f)
                                (current-require-path-suffixes '("sls"))
                                (current-require-path-suffixes-compiled
                                 '("slfasl")))
                               (let ((fname
                                      ((current-library-resolver) name)))
                                 (if fname
                                     (return fname))))))
                          libpaths))
              require-paths)
             #f))))
    (if fname
        (load-r6rs-library fname)
        (assertion-violation 'lookup-library "library not loaded" libname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expands an R6RS program into an R5RS program that can be
; loaded by load-r6rs-program.  The target-filename can be
; compiled before it is loaded.

(define (expand-r6rs-program filename target-filename)
  (larceny:load-r6rs-package)
  (ex:expand-file filename target-filename))

; Loads (thereby running) an expanded R6RS program
; or a .fasl file compiled from an expanded R6RS program.
;
; FIXME:  Nothing uses this, so it is commented out.

;(define (load-r6rs-program filename)
;  (larceny:load-r6rs-runtime)
;  (load filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Imported by (larceny compile-file).

(define (compile-r6rs-file src dst libraries-only?)

  (cond ((and libraries-only?
              (not (contains-libraries-only? src)))
         (assertion-violation
          'compile-library
          "contains non-library code" src))
        (dst
         (let ((tempfile (generate-temporary-name dst)))
(display "Compiling ")
;(newline)
(display src)
(newline)
;(display tempfile)
;(newline)
;(display dst)
;(newline)
           (dynamic-wind
            (lambda () #t)
            (lambda ()
              (parameterize ((fasl-evaluator aeryn-fasl-evaluator)
                             (load-evaluator aeryn-evaluator)
                             (repl-evaluator aeryn-evaluator))
                (expand-r6rs-program src tempfile))
              (compile-file tempfile dst))
            (lambda () (delete-file tempfile)))))
        (else
         (compile-r6rs-file src
                            (generate-fasl-name src)
                            libraries-only?))))

(define (contains-libraries-only? fn)
  (let* ((nothing-but-libraries?
          (lambda (in)
            (do ((x (read in) (read in)))
                ((or (eof-object? x)
                     (and (not (eq? x (unspecified))) ; flags are permitted
                          (not (and (pair? x)
                                    (eq? 'library (car x))))))
                 (eof-object? x)))))
         (nothing-but-libraries?
          (make-file-processer/preserve-reader-state nothing-but-libraries?)))
    (call-with-input-file fn nothing-but-libraries?)))

(define (generate-temporary-name fn)
  (let* ((n (random 1000000))
         (s (string-append fn "." (number->string n 16))))
    (if (file-exists? s)
        (generate-temporary-name fn)
        s)))

(define (generate-fasl-name fn)
  (rewrite-file-type fn
                     *scheme-file-types*
                     *slfasl-file-type*))

(define *scheme-file-types* '(".sls" ".sch" ".scm"))
(define *slfasl-file-type*    ".slfasl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: from src/Compiler/driver-common.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a file name with some type, produce another with some other type.

(define (rewrite-file-type filename matches new)
  (if (not (pair? matches))
      (rewrite-file-type filename (list matches) new)
      (let ((j (string-length filename)))
        (let loop ((m matches))
          (cond ((null? m)
                 (string-append filename new))
                (else
                 (let* ((n (car m))
                        (l (string-length n)))
                   (if (file-type=? filename n)
                       (string-append (substring filename 0 (- j l)) new)
                       (loop (cdr m))))))))))

(define (file-type=? file-name type-name)
  (let ((fl (string-length file-name))
        (tl (string-length type-name)))
    (and (>= fl tl)
         (string=? type-name
                   (substring file-name (- fl tl) fl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: from src/Compiler/driver-larceny.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-file-processer/preserve-reader-state a-process-file)
  (lambda args
   (apply a-process-file args)))
