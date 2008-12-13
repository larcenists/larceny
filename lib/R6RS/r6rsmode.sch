; Copyright 2007 William D Clinger
;
; $Id$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Outline:
;
; mode-specific evaluators
; R6RS initialization
; R6RS entry points
; autoloading of libraries
; compilation of library files
; reading of library files
; R6RS-specific file/directory/pathname hacking
; general file, directory, and pathname hacking
;     (FIXME: should be moved elsewhere)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mode-specific evaluators.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ERR5RS and R6RS programs may need to require or to load
; R5RS libraries.

(define r5rs:load-evaluator (load-evaluator))

(define (r5rs:load . args)
  (parameterize ((load-evaluator r5rs:load-evaluator))
    (apply load args)))

(define (r5rs:require . args)
  (parameterize ((load-evaluator r5rs:load-evaluator))
    (apply require args)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS initialization.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compiles Larceny's basic support for ERR5RS/R6RS and the
; standard ERR5RS/R6RS libraries.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; larceny:load-r6rs-runtime doesn't seem to be called,
; but it might be useful when running an application that
; doesn't need eval.

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
;
; R6RS entry points.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Called by interactive-entry-point in Lib/Repl/main.sch

(define (run-r6rs-forms forms)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-sequence forms))

; Called by interactive-entry-point in Lib/Repl/main.sch
;
; If a Scheme script (but not a top-level R6RS program!)
; begins with "#!\" or "#! ", then the first line should
; be ignored.  Ugh.
; 
; Fasl files must be raw Latin-1, while source files may be
; UTF-8 or UTF-16 on some platforms.
; To detect fasl files, we open the file as raw Latin-1 and
; look at the first line.  If it's a fasl file, we load it
; as a fasl file.  Otherwise we close the raw Latin-1 port
; and load the file as a source file.

(define (run-r6rs-program filename)
  (larceny:load-r6rs-package)
  (cond ((cdr (assq 'ignore-first-line (system-features)))
         (call-with-port
          (open-input-file filename)
          (lambda (p)
            (get-line p)
            (load-from-port p interaction-environment))))
        ((call-with-port
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
         (unspecified))
        (else
         (ex:run-r6rs-program filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Auto-loading and auto-compiling of ERR5RS/R6RS libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (larceny:register! fname)
  (if (absolute-path-string? fname)
      (begin (set! larceny:autoloaded-r6rs-library-files
                   (cons fname larceny:autoloaded-r6rs-library-files))
             ;(display "    from ")
             ;(display fname)
             ;(newline)
             #t)
      (larceny:register!
       (larceny:absolute-path fname))))

; Called by ex:lookup-library in lib/R6RS/r6rs-runtime.sch
; Returns false if the library has previously been loaded,
; returns true if the file exists and loads successfully,
; or raises an exception.

(define (larceny:autoload-r6rs-library libname)

  (define (load-r6rs-library fname)
    (cond ((member fname larceny:autoloaded-r6rs-library-files)
           #f)
          ((compile-libraries-older-than-this-file)
           =>
           (lambda (reference-file)
             (cond ((file-type=? fname ".sls")
                    (compile-r6rs-file fname #f #t)
                    (let ((fname (rewrite-file-type fname ".sls" ".slfasl")))
                      (larceny:register! fname)
                      (load fname)
                      #t))
                   ((file-type=? fname ".slfasl")
                    (let ((src (rewrite-file-type fname ".slfasl" ".sls")))
                      (if (and (file-exists? src)
                               (or (file-newer? src fname)
                                   (and
                                    (in-same-directory? reference-file fname)
                                    (file-newer? reference-file fname))))
                          (begin
                                 (compile-r6rs-file src fname #t)
                                 (larceny:register! fname)
                                 (load fname)
                                 #t)
                          (begin (larceny:register! fname)
                                 (load fname)
                                 #t))))
                   (else
                    (larceny:register! fname)
                    (load fname)
                    #t))))
          (else
           (larceny:register! fname)
           (load fname)
           #t)))

  (if (eq? (cdr (assq 'execution-mode (system-features)))
           'err5rs)
      (begin (display "Autoloading ")
             (write libname)
             (newline)))

  (let* ((libpath (map symbol->string libname))
         (libpath (map larceny:filename-mangler libpath))
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
                                (current-require-path-suffixes
                                 *library-suffixes-source*)
                                (current-require-path-suffixes-compiled
                                 *library-suffixes-compiled*))
                               (let ((fname
                                      ((current-library-resolver) name)))
                                 (if fname
                                     (return fname))))))
                          libpaths))
              require-paths)
             #f))))
    (if fname
        (parameterize ((larceny:r6rs-expand-only #f))
          (load-r6rs-library fname))
        (assertion-violation 'lookup-library "library not loaded" libname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expands an R6RS program into an R5RS program that can be
; loaded by load-r6rs-program.  The target-filename can be
; compiled before it is loaded.
;
; FIXME:  This uses a hack to avoid compiling a library twice
; in systems that compile on eval.  If the file contains only
; one library, and nothing else, then we can compile to a file
; and then load the file instead of calling eval on the result
; of expansion.  That doesn't work if the file contains two
; libraries and the second one imports from the first.
;
; FIXME:  The io here should be protected against errors.

(define (expand-r6rs-program filename target-filename)
  (larceny:load-r6rs-package)
  (let ((nlibs (call-with-input-file
                filename
                (lambda (in)
                  (do ((x (read in) (read in))
                       (n 0 (if (pair? x) (+ n 1) n)))
                      ((or (eof-object? x) (> n 1))
                       n))))))
    (parameterize ((larceny:r6rs-expand-only (= nlibs 1)))
      (ex:expand-file filename target-filename))))

; This parameter determines whether expanded libraries
; and programs are evaluated immediately, as in van Tonder's
; original code, or just expanded.

(define larceny:r6rs-expand-only
  (make-parameter "larceny:r6rs-expand-only" #f boolean?))

; Loads (thereby running) an expanded R6RS program
; or a .fasl file compiled from an expanded R6RS program.
;
; FIXME:  Nothing uses this, so it is commented out.

;(define (load-r6rs-program filename)
;  (larceny:load-r6rs-runtime)
;  (load filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Compilation of ERR5RS/R6RS libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; When (rnrs base) is recompiled, then all of the older .slfasl
; files need to be recompiled.
; FIXME:  Should limit to files that depend on the recompiled file(s).

(define compile-libraries-older-than-this-file
  (make-parameter "compile-libraries-older-than-this-file" #f))

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
                                (let ((slfasl
                                       (and (file-type=? file ".sls")
                                            (let ((slfasl
                                                   (rewrite-file-type
                                                    file ".sls" ".slfasl")))
                                              (cond ((not
                                                      (file-exists? slfasl))
                                                     slfasl)
                                                    ((file-newer? basefile
                                                                  slfasl)
                                                     slfasl)
                                                    (else #f))))))
                                  (if slfasl
                                      (begin (compile-r6rs-file file slfasl #t)
                                             (larceny:register! slfasl)
                                             (load slfasl)))))
                              files)))))
          (compile-libraries path)))
       (else
        (larceny:unsupported-os))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reading of library files.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS-specific file/directory/pathname hacking.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suffixes that get rewritten when compiling a file.

(define *scheme-file-types* '(".sls" ".sch" ".scm"))
(define *slfasl-file-type*    ".slfasl")

; Suffixes recognized during the search for ERR5RS/R6RS libraries.
; Note: no initial period here.

(define *library-suffixes-source*   '("larceny.sls"    "sls"))
(define *library-suffixes-compiled* '("larceny.slfasl" "slfasl"))

(define (generate-fasl-name fn)
  (rewrite-file-type fn
                     *scheme-file-types*
                     *slfasl-file-type*))

; Given a string that is one component of an ERR5RS/R6RS library,
; replaces any funny characters by a possibly Larceny-specific
; sequence of characters that are more likely to be accepted by
; the file system.
;
; FIXME: for now, just translates #\: to "%3a".

(define (larceny:filename-mangler s)
  (do ((rchars (reverse (string->list s)) (cdr rchars))
       (chars '()
              (cond ((char=? (car rchars) #\:)
                     (append (string->list "%3a") chars))
                    (else (cons (car rchars) chars)))))
      ((null? rchars)
       (list->string chars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File, directory, and pathname hacking.
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

; Given a list of strings representing directory and file names,
; returns a single string representing the entire path.

(define (make-path path)
  (apply string-append
         (cons (car path)
               (map (lambda (s) (string-append (larceny:separator) s))
                    (cdr path)))))

; Returns true iff the second file is in the same directory
; as the first, or in a subdirectory of that directory.

(define (in-same-directory? fname1 fname2)
  (define (string-prefix? s1 s2)
    (let ((n1 (string-length s1))
          (n2 (string-length s2)))
      (and (<= n1 n2)
           (string=? s1 (substring s2 0 n1)))))

  (string-prefix? (larceny:directory-of fname1)
                  (larceny:directory-of fname2)))

; Given some prefix string, returns the prefix with a randomly
; selected suffix that doesn't conflict with any existing file.

(define (generate-temporary-name fn)
  (let* ((n (random 1000000))
         (s (string-append fn "." (number->string n 16))))
    (if (file-exists? s)
        (generate-temporary-name fn)
        s)))

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

; Converts file names to absolute paths.
;
; FIXME: doesn't convert foo/bar/.. to foo.

(define (larceny:absolute-path fname)
  (if (absolute-path-string? fname)
      fname
      (string-append (larceny:directory-of fname)
                     (larceny:separator)
                     (larceny:file-name-only fname))))

; FIXME: assumes (larceny:separator) is one character long.

(define (larceny:file-name-only fname)
  (let ((sep (string-ref (larceny:separator) 0)))   ; FIXME
    (let loop ((chars (string->list fname)))
      (let ((probe (memv sep chars)))
        (if probe
            (loop (cdr probe))
            (list->string chars))))))

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
