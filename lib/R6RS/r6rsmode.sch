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

; R7RS and R6RS programs may need to require or to load
; R5RS libraries.

(define r5rs:load-evaluator (load-evaluator))

(define (r5rs:load . args)
  (parameterize ((load-evaluator r5rs:load-evaluator))
    (apply load args)))

(define (r5rs:require . args)
  (parameterize ((load-evaluator r5rs:load-evaluator))
    (apply require args)))

; Larceny's R7RS and R6RS modes.
; Code names:
;     Aeryn    R7RS and ERR5RS
;     D'Argo   R6RS-compatible
;     Spanky   R6RS-conforming (not yet implemented)
;
; R7RS is implemented as a veneer over ERR5RS.
; The only difference between R7RS and ERR5RS modes is that
; R7RS mode automatically imports at least (scheme base) while
; ERR5RS imports nothing.

(define (aeryn-evaluator exp . rest)
  (ex:repl (list exp)))

(define aeryn-fasl-evaluator
  (let* ((arch-name (cdr (assq 'arch-name (system-features))))
         (fasl-value
          (cond ((string=? arch-name "Standard-C")
                 '(import (rnrs base)
                          (primitives .petit-shared-object
                                      .petit-patch-procedure)))
                ((string=? arch-name "CLR")
                 '(import (rnrs base)
                          (primitives @common-patch-procedure)))
                (else
                 (unspecified)))))
    (lambda () fasl-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS/R7RS initialization.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compiles Larceny's basic support for ERR5RS/R6RS/R7RS and the
; standard ERR5RS/R6RS/R7RS libraries.
;
; FIXME: Should use the (current-require-path) instead of
; hard-wiring names of the standard directories that contain
; ERR5RS/R6RS/R7RS libraries, but the (current-require-path)
; might contain directories that overlap, causing libraries
; to be compiled twice, causing build inconsistencies.

(define (larceny:compile-r6rs-runtime)
  (parameterize ((current-directory (current-larceny-root)))
    (define (compile-r6rs-runtime-core)
      (parameterize ((current-directory
                      (larceny:canonical-path "lib/R6RS")))
        (compile-file "r6rsmode.sch")
        (compile-file "../R7RS/r7rs-includer.sch")                      ; FIXME
        (compile-file "../R7RS/r7rs-cond-expander.sch")                 ; FIXME
        (compile-file "r6rs-compat-larceny.sch")
        (compile-file "r6rs-runtime.sch")
        (compile-file "r6rs-expander.sch")
        (require 'r7rs-includer)                                        ; FIXME
        (require 'r7rs-cond-expander)                                   ; FIXME
        (require 'r6rs-compat-larceny)
        (require 'r6rs-runtime)
        (require 'r6rs-expander)
        (ex:expand-file "r6rs-standard-libraries.sch"
                        "r6rs-standard-libraries.exp")
        (compile-file "r6rs-standard-libraries.exp"
                      "r6rs-standard-libraries.fasl")
        (require 'r6rs-standard-libraries)))
    (define (compile-source-libraries)
      (for-each (lambda (dir)
                  (parameterize ((current-directory
                                  (larceny:canonical-path dir)))
                    (compile-stale-libraries)))
                    '("lib")))                                          ; FIXME
    (time (compile-r6rs-runtime-core))
    (time (compile-source-libraries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; larceny:load-r6rs-runtime doesn't seem to be called,
; but it might be useful when running an application that
; doesn't need eval.

(define *r6rs-runtime-is-loaded* #f)
(define *r6rs-package-is-loaded* #f)

(define (larceny:load-r6rs-runtime)
  (if (not *r6rs-runtime-is-loaded*)
      (begin
       (set! *r6rs-runtime-is-loaded* #t)
       (require 'r6rs-compat-larceny)
       (require 'r6rs-runtime)
       (require 'r6rs-standard-libraries))))

(define (larceny:load-r6rs-package)
  (if (not *r6rs-package-is-loaded*)
      (begin (set! *r6rs-package-is-loaded* #t)
             (set! *r6rs-runtime-is-loaded* #t)
             (require 'r7rs-includer)                                 ; FIXME
             (require 'r7rs-cond-expander)
             (require 'r6rs-compat-larceny)
             (require 'r6rs-runtime)
             (require 'r6rs-expander)
             (require 'r6rs-standard-libraries))))

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

                     ;; FIXME: resetting the port to position 0
                     ;; should have the same effect

                     ;; (aeryn-evaluator (aeryn-fasl-evaluator)
                     ;;                  interaction-environment)

                     (set-port-position! p 0)
                     (load-from-port p interaction-environment)
                     #t)
                    (else
                     #f)))))
         (unspecified))
        (else
         (ex:run-r6rs-program filename))))

(define (load-r6rs-library-or-program filename . rest)
  (define env (and (pair? rest) (car rest)))
  (larceny:load-r6rs-package)
  (cond ((call-with-port
          (open-raw-latin-1-input-file filename)
          (lambda (p)
            (let ((first-line (get-line p)))
              (cond ((and (not env)
                          (string? first-line)
                          (string=? first-line "#!fasl"))

                     ;; FIXME: resetting the port to position 0
                     ;; should have the same effect

                     ;; (aeryn-evaluator (aeryn-fasl-evaluator)
                     ;;                  interaction-environment)

                     (set-port-position! p 0)
                     (load-from-port p interaction-environment)
                     #t)
                    (else
                     #f)))))
         (unspecified))
        (else
         (let* ((srcdir (larceny:directory-of filename))
                (paths (current-require-path))
                (paths (if (member srcdir paths)
                           paths
                           (cons srcdir paths))))
           (parameterize ((current-require-path paths))
            (if env
                (ex:load filename env)
                (ex:load filename)))))))

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

(define larceny:autoloaded-r6rs-library-files '())

(define (larceny:register! fname)
  (let ((path (larceny:absolute-path fname)))
    (if (larceny:registered? fname)
        (begin (display "Already registered: ")
               (write path)
               (newline)
               (error 'larceny:register!
                      (string-append
                       "circular dependency between library files\n"
                       "(putting each library in its own file might help)")
                      fname)))
    (set! larceny:autoloaded-r6rs-library-files
          (cons path
                larceny:autoloaded-r6rs-library-files))))

(define (larceny:registered? fname)
  (member (larceny:absolute-path fname)
          larceny:autoloaded-r6rs-library-files))

; Called by ex:lookup-library in lib/R6RS/r6rs-runtime.sch
; Returns false if the library has previously been loaded,
; returns true if the file exists and loads successfully,
; or raises an exception.

(define (larceny:autoload-r6rs-library libname)

  (define (load-r6rs-library fname)
    (cond ((larceny:registered? fname)
           #f)
          ((compile-libraries-older-than-this-file)
           =>
           (lambda (reference-file)
             (cond ((exists (lambda (suffix) (file-type=? fname suffix))
                            *library-file-types*)
                    (compile-r6rs-file fname #f #t)
                    (let ((fasl (generate-fasl-name fname)))
                      (larceny:register! fname)
                      (larceny:register! fasl)
                      (load fasl)
                      #t))
                   ((file-type=? fname *slfasl-file-type*)
                    (let loop ((srcnames (generate-source-names fname)))
                      (if (null? srcnames)
                          ;; FIXME: compiled file, no matching source
                          (begin (larceny:register! fname)
                                 (load fname)
                                 #t)
                          (let ((src (car srcnames)))
                            (if (and (file-exists? src)
                                     (or (file-newer? src fname)
                                         (and
                                          (in-same-directory? reference-file
                                                              fname)
                                          (file-newer? reference-file fname))))
                                (begin (larceny:register! src)
                                       (compile-r6rs-file src fname #t)
                                       (larceny:register! fname)
                                       (load fname)
                                       #t)
                                (loop (cdr srcnames)))))))
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

  (let ((fname (larceny:find-r6rs-library libname)))
    (if fname
        (let* ((srcdir (larceny:directory-of fname))
               (paths (current-require-path))
               (paths (if (member srcdir paths)
                          paths
                          (cons srcdir paths))))
          (parameterize ((larceny:r6rs-expand-only #f)
                         (current-require-path paths))
            (load-r6rs-library fname)))
        (assertion-violation 'lookup-library "library not loaded" libname))))

;;; Returns the name of a file that can reasonably be expected
;;; to define the given library, or returns #f if no such file
;;; is found.

(define (larceny:find-r6rs-library libname)

  (let* ((->string (lambda (x)
                     (cond ((symbol? x) (symbol->string x))
                           ((number? x) (number->string x))
                           (else
                            (error 'larceny:find-r6rs-library
                                   "bad library name"
                                   libname)))))
         (libpath (map ->string libname))
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
                                 (if (and fname
                                          (larceny:find-r6rs-library-really?
                                           libname
                                           fname))
                                     (return fname))))))
                          libpaths))
              require-paths)
             #f))))
    fname))

;;; Given the name of an R7RS/R6RS library and the name of the file
;;; in which the autoloader expects to find it (based on file naming
;;; conventions), returns true if and only if the library is actually
;;; defined within the file.
;;;
;;; FIXME: for compiled files, this remains heuristic.

(define (larceny:find-r6rs-library-really? libname fname)

  (define (search-source-library-file fname)
    (call-without-errors
     (lambda ()
       (call-with-input-file
        fname
        (lambda (p)
          (let loop ()
            (let ((x (read p)))
              (cond ((eof-object? x)
                     #f)
                    ((and (pair? x)
                          (memq (car x) *library-keywords*)
                          (pair? (cdr x))
                          (equal? (larceny:libname-without-version (cadr x))
                                  (larceny:libname-without-version libname)))
                     #t)
                    (else (loop))))))))))

  (cond ((file-type=? fname *slfasl-file-type*)
         (let ((srcnames (generate-source-names fname)))

           ;; FIXME: if there is no corresponding source file,
           ;; then we'll just assume the library is defined by
           ;; the compiled file.

           (or (null? srcnames)
               (let loop ((srcnames (generate-source-names fname)))
                 (cond ((null? srcnames) #f)
                       ((file-exists? (car srcnames))
                        (search-source-library-file (car srcnames)))
                       (else (loop (cdr srcnames))))))))
        (else (search-source-library-file fname))))

;;; Given the name of an R7RS/R6RS library, returns the name without
;;; version numbers.

(define (larceny:libname-without-version libname)
  (if (and (pair? libname)
           (list? (car (last-pair libname))))
      (reverse (cdr (reverse libname)))
      libname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expands an R6RS program into an R5RS program that can be
; loaded by load-r6rs-program.  The target-filename can be
; compiled before it is loaded.
;
; FIXME:  The io here should be protected against errors.

(define (expand-r6rs-program filename target-filename)
  (larceny:load-r6rs-package)
  (ex:expand-file filename target-filename))

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
; compiles all ERR5RS/R6RS library files in that directory and its
; subdirectories that are older than the reference file.
;
; The reference file is typically a file that has been modified,
; so all files that depend upon it must be recompiled anyway.
;
; FIXME: It might be better to compile only the files that depend
; upon the reference file, but that's a little harder.

(define (compile-stale-libraries . rest)
  (cond ((null? rest)
         (let* ((fname (generate-temporary-name
                        (string-append (current-directory) "/temporary")))
                (fname (larceny:canonical-path fname)))
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
    (define (compiled-name file)
      (and (exists (lambda (suffix) (file-type=? file suffix))
                   *library-file-types*)
           (not (larceny:excluded-file-name? file))
           (let ((slfasl
                  (generate-fasl-name file)))
             (cond ((not (file-exists? slfasl))
                    slfasl)
                   ((file-newer? basefile slfasl)
                    slfasl)
                   (else #f)))))
    (parameterize ((fasl-evaluator aeryn-fasl-evaluator)
                   (load-evaluator aeryn-evaluator)
                   (repl-evaluator aeryn-evaluator))
      (case (larceny:os)
       ((unix windows)
        (let ()
          (define (compile-libraries path)
            (if (larceny:directory? path)
                (let* ((files (larceny:list-directory path))
                       (files (or files '())) ; be careful here
                       (files (larceny:sort-by-suffix-priority files)))
                  (parameterize ((current-directory path))
                    (for-each (lambda (file)
                                (if (larceny:directory? file)
                                    (compile-libraries file)))
                              files)
                    (for-each (lambda (file)
                                (let ((slfasl (compiled-name file)))
                                  (if slfasl
                                      (begin (larceny:register! file)
                                             (compile-r6rs-file file slfasl #t)
                                             (larceny:register! slfasl)
                                             (load slfasl)))))
                              files)))))
          (compile-libraries path)))
       (else
        (larceny:unsupported-os))))))

; FIXME: As reported in ticket #602, it's annoying when Larceny
; compiles foo.larceny.sls and also compiles foo.vicare.sls in
; the same directory.  As a temporary workaround, we won't compile
; a file named X.Y.sls or X.Y.sld if a file named X.larceny.sls or
; X.larceny.sld exists in the same directory.

(define (larceny:excluded-file-name? file)
  (and (or (file-type=? file ".sld")
           (file-type=? file ".sls"))
       (let* ((revchars (reverse (string->list file)))
              (period1 (memv #\. revchars))
              (period2 (and period1 (memv #\. (cdr period1))))
              (n (and period2 (length (cdr period2))))
              (x (and n (substring file 0 n))))
         (and x
              (or (file-exists? (string-append x ".larceny.sld"))
                  (file-exists? (string-append x ".larceny.sls")))
              (not (string=? (substring file 0 (- (string-length file) 4))
                             (string-append x ".larceny")))))))

; Imported by (larceny compile-file).

(define (compile-r6rs-file src dst libraries-only?)

  (cond ((and libraries-only?
              (not (larceny:contains-libraries-only? src)))
         (assertion-violation
          'compile-library
          "contains non-library code" src))
        (dst
         (let* ((tempfile (generate-temporary-name dst))
                (srcdir (larceny:directory-of src))
                (paths (current-require-path))
                (paths (if (member srcdir paths)
                           paths
                           (cons srcdir paths))))
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
                             (repl-evaluator aeryn-evaluator)
                             (current-require-path paths))
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

(define *library-keywords* '(library define-library))

(define (larceny:contains-libraries-only? fn)
  (let* ((nothing-but-libraries?
          (lambda (in)
            (do ((x (read in) (read in)))
                ((or (eof-object? x)
                     (and (not (eq? x (unspecified))) ; flags are permitted
                          (not (and (pair? x)
                                    (memq (car x) *library-keywords*)))))
                 (eof-object? x))))))
    (call-without-errors
     (lambda ()
       (call-with-input-file fn nothing-but-libraries?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS-specific file/directory/pathname hacking.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Suffixes recognized during the search for R7RS/R6RS libraries.
;
; Order matters.  If "sld" is listed before "sls", then a file
; named "foo.sld" will be used instead of a file named "foo.sls".
; Although "foo.sld" and "foo.sls" both compile to "foo.slfasl",
; compile-libraries and compile-stale-libraries will compile
; "foo.sld" before "foo.sls" and will then skip compilation of
; "foo.sls" because "foo.slfasl" will already exist.

; Note: no initial periods here.

(define *library-suffixes-source*
  '("larceny.sld" "larceny.sls" "sld" "sls"))

(define *library-suffixes-compiled*
  '("larceny.slfasl" "slfasl"))

(define (larceny:sort-by-suffix-priority filenames)
  (define (priority filename suffixes)
    (cond ((null? suffixes) 0)
          ((file-type=? filename (car suffixes))
           (length suffixes))
          (else
           (priority filename (cdr suffixes)))))
  (define (greater? fn1 fn2)
    (let ((p1 (priority fn1 *library-suffixes-source*))
          (p2 (priority fn2 *library-suffixes-source*)))
      (cond ((> p1 p2) #t)
            ((= p1 p2) (string<? fn1 fn2))
            (else #f))))
  (list-sort greater? filenames))

; Suffixes that get rewritten when compiling a file.
; There is only one suffix we need to recognize for compiled libraries,
; and we need to keep it that way or autoloading will get out of hand.

(define *library-file-types* '(".sld" ".sls"))
(define *scheme-file-types*  (append *library-file-types* '(".sch" ".scm")))
(define *slfasl-file-type*   ".slfasl")

(define (generate-fasl-name fn)
  (rewrite-file-type fn
                     *scheme-file-types*
                     *slfasl-file-type*))

; Returns a list of all source file names that generate-fasl-name
; might have converted to the given fasl name.

(define (generate-source-names fn)
  (map (lambda (suffix)
         (rewrite-file-type fn *slfasl-file-type* suffix))
       *library-file-types*))

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

; On Unix systems, this procedure just returns its argument.
;
; On Windows, this procedure converts pathnames to canonical
; form by replacing forward slashes with backslashes
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
  (case (larceny:os)
   ((windows)
    (loop (reverse (string->list path)) '() #f))
   (else path)))

; Converts file names to absolute paths.
;
; FIXME: doesn't convert foo/bar/.. to foo.
;
; FIXME: must call larceny:directory-of to work around OS renaming

(define (larceny:absolute-path fname)
  (string-append (larceny:directory-of fname)
                 (larceny:separator)
                 (larceny:file-name-only fname)))

; FIXME: assumes (larceny:separator) is one character long.

(define (larceny:file-name-only fname)
  (let ((sep (string-ref (larceny:separator) 0)))   ; FIXME
    (let loop ((chars (string->list fname)))
      (let ((probe (memv sep chars)))
        (if probe
            (loop (cdr probe))
            (list->string chars))))))

;;; Attempts to use Larceny's built-in list-directory, but
;;; that procedure may not work on all platforms.  Falls back
;;; on using ls or dir to write directories to a temporary
;;; file, which can then be read.
;;;
;;; That's almost okay when compiling, which is the only use
;;; for this procedure, because the compiler will probably
;;; write into the directory anyway.
;;;
;;; This procedure is always called during execution of
;;; compile-r6rs-runtime, so the assignments seen below
;;; will always be performed at that time.  The directory
;;; listed at that time should never be empty.  If it is,
;;; a warning message will be printed and the runtime will
;;; still be compiled.

(define larceny:list-directory
  (lambda (path)
    (let ((files (list-directory path)))
      (if (or (not files) (null? files) (boolean? (car files))) ; FIXME
          (begin (newline)
                 (display "***** list-directory doesn't work *****\n")
                 (display files)
                 (display "Falling back on ls or dir\n\n")
                 (set! files (larceny:list-directory-using-ls path))
                 (set! larceny:list-directory
                       larceny:list-directory-using-ls))
          (set! larceny:list-directory
                larceny:list-directory-using-syscalls))
      files)))

(define (larceny:list-directory-using-syscalls path)
  (list-directory path))

;;; FIXME:  This writes into the current directory.

(define (larceny:list-directory-using-ls path)
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
                 (string-append "ls -1 " path " > '" tempfile "'")))
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

;;; FIXME: larceny:list-subdirectories is no longer used by anyone,
;;; so it's commented out.

#;
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
   ((unix windows)
    (and (list-directory path) #t))
   (else
    (larceny:unsupported-os))))

; Under Windows, converts to a canonical form that
; replaces backward slashes by forward slashes
; and ignores all but the first of a sequence of consecutive slashes.
; FIXME: doesn't handle . or ..
;
; FIXME: the parameterize form is a workaround for OS conversions
; from /tmp to /private/tmp and the like.

(define (larceny:directory-of fname)
  (case (larceny:os)
   ((unix)
    (if (absolute-path-string? fname)
        (do ((i (- (string-length fname) 1) (- i 1)))
            ((char=? (string-ref fname i) #\/)
             (parameterize ((current-directory (substring fname 0 i))) ; FIXME
               (current-directory))))
        (larceny:directory-of (string-append (current-directory) "/" fname))))
   ((windows)
    (if (absolute-path-string? fname)
        (do ((i (- (string-length fname) 1) (- i 1)))
            ((or (< i 0)
                 (memv (string-ref fname i) '(#\\ #\/)))
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

; eof
