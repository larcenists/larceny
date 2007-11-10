; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny's ERR5RS and R6RS modes.
; Code names:
;     Aeryn    ERR5RS
;     D'Argo   R6RS-compatible
;     Spanky   R6RS-conforming (not yet implemented)

; FIXME:  This is for compiling by hand.  It should be moved into
; test/Scripts/package-bin-release.sh
;
; FIXME:  This assumes the current directory is lib/R6RS

(define (larceny:compile-r6rs-runtime)
  (compile-file "r6rs-compat-larceny.sch")
  (compile-file "r6rs-runtime.sch")
  (compile-file "r6rs-expander.sch")
  (load "r6rs-compat-larceny.fasl")
  (load "r6rs-runtime.fasl")
  (load "r6rs-expander.fasl")
  (ex:expand-file "r6rs-standard-libraries.sch" "r6rs-standard-libraries.exp")
  (compile-file "r6rs-standard-libraries.exp" "r6rs-standard-libraries.fasl"))

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

(define (run-r6rs-program filename)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-program filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expands an R6RS program into an R5RS program that can be
; loaded by load-r6rs-program.  The target-filename can be
; compiled before it is loaded.

(define (expand-r6rs-program filename target-filename)
  (larceny:load-r6rs-package)
  (ex:expand-file filename target-filename))

; Loads (thereby running) an expanded R6RS program
; or a .fasl file compiled from an expanded R6RS program.

(define (load-r6rs-program filename)
  (larceny:load-r6rs-runtime)
  (load filename))

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
(newline)
(display src)
(newline)
(display tempfile)
(newline)
(display dst)
(newline)
           (dynamic-wind
            (lambda () #t)
            (lambda ()
              (expand-r6rs-program src tempfile)
              (compile-file tempfile dst))
            (lambda () (delete-file tempfile)))))
        (else
         (compile-r6rs-file src (generate-fasl-name src) libraries-only?))))

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
                     *fasl-file-type*))

(define *scheme-file-types* '(".sls" ".sch" ".scm"))
(define *fasl-file-type*    ".slfasl")

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

  ;; FIXME: The reader modes are parameterized here to protect the
  ;; interactive session's modes from changes made while reading the
  ;; compiled file.
  ;; FIXME: This needs to be kept in sync with the preserved
  ;; parameters in src/Lib/Common/load.sch until we adopt a more
  ;; robust solution.

  (lambda args
    (parameterize
     ((recognize-keywords?          (recognize-keywords?))
      (recognize-javadot-symbols?   (recognize-javadot-symbols?))
      (read-square-bracket-as-paren (read-square-bracket-as-paren))
      (case-sensitive?              (case-sensitive?))
      (read-r6rs-flags?             #t)
      (read-larceny-weirdness?      (read-larceny-weirdness?))
      (read-traditional-weirdness?  (read-traditional-weirdness?))
      (read-mzscheme-weirdness?     (read-mzscheme-weirdness?)))
     (apply a-process-file args))))
