;;; Copyright 2015 William D Clinger
;;;
;;; $Id$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements the file-related parts of these features of
;;; R7RS define-library:
;;;
;;;     include
;;;     include-ci
;;;     include-library-declarations
;;;
;;; The code in this file assumes lib/R6RS/r6rsmode.sch has
;;; been loaded.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Procedures called by lib/R6RS/r6rs-expander.sch
;;;
;;; If all files are found, each larceny:include* procedure
;;; returns a list that looks like
;;;
;;;     (begin
;;;      (begin #\I "included from " <dirname1> <basename1> <form> ...)
;;;      ...)
;;;
;;; where
;;;
;;;     #\I is the character (see rationale below)
;;;     "included from " (with the space) is allocated below
;;;     <dirname1> is the dirname where the first file was found
;;;     <basename1> is the basename where the first file was found
;;;
;;; and so on for the other files.
;;;
;;; Rationale:
;;;
;;; That begin form is legal within expression contexts, and
;;; Larceny allows it within top-level definition contexts other
;;; than R6RS libraries, which don't allow include anyway.
;;;
;;; The #\I will be uncommon in that position, and checking for
;;; it is easy and fast.  Code that needs to recognize included
;;; forms can perform that check to avoid calling
;;; larceny:included-forms? in the common case.
;;;
;;; The macro expander can use the <dirname> to allow
;;; self-relative include forms within included files.
;;;
;;; The <basename> can be attached to source code location.
;;;
;;; FIXME: The R6RS/R7RS macro expander doesn't track source
;;; code locations, but basic infrastructure for that is
;;; provided by Larceny's reader and some source locations
;;; are tracked in R5RS mode.  See
;;;
;;;     src/Lib/Common/load.sch (read-source-code)
;;;     src/Lib/Common/procinfo.sch (source-location-recorder)
;;;     src/Compiler/driver-larceny.sch (twobit:source-location-recorder)
;;;     src/Compiler/pass1.sch (pass1-lookup-source-position etc)
;;;     src/Compiler/expand.sch
;;;
;;; At the very least, the R6RS/R7RS macro expander ought to do
;;; what the R5RS macro expander is doing with source locations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (larceny:include filenames)
  (cons 'begin
        (map larceny:include-file filenames)))

(define (larceny:include-ci filenames)
  (cons 'begin
        (map larceny:include-ci-file filenames)))

(define (larceny:include-library-declarations filenames)
  (cons 'begin
        (map larceny:include-library-declarations-file filenames)))

;;; Returns true if and only its argument is of the form
;;;
;;;     (begin #\I <token> <dirname> <basename> <form> ...)
;;;
;;; where <token> is a specific object allocated below.

(define (larceny:included-forms? x)
  (and (list? x)
       (<= 5 (length x))
;      (eq? (car x) 'begin)    ; FIXME
       (eq? (cadr x) #\I)
       (eq? (caddr x) *inclusion-token*)))

;;; Given a begin form that satisfies the predicate above,
;;; returns a list of the forms that follow the <basename>.
;;; Otherwise returns #f.

(define (larceny:included-forms-as-list x)
  (if (larceny:included-forms? x)
      (cddr (cdddr x))
      #f))

;;; Returns two values.
;;;
;;; Given a begin form that satisfies larceny:included-forms?,
;;; returns the <dirname> and <basename>.
;;;
;;; Otherwise returns #f and #f.

(define (larceny:source-path-of-included-forms x)
  (if (larceny:included-forms? x)
      (let ((y (cdddr x)))
        (values (car y) (cadr y)))
      (values #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private to this file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Unforgeable object used to mark begin forms returned by
;;; the larceny:include procedures.

(define *inclusion-token* (string-copy "included from "))

(define (larceny:make-included-begin-form forms dirname basename)
  (append (list 'begin #\I *inclusion-token* dirname basename)
          forms))

;;; Suffixes recognized during the search for R7RS source files
;;; to be included within an R7RS define-library (so the "sls"
;;; suffix would be quite wrong here).
;;;
;;; FIXME: These lists should be empty, because R7RS (small)
;;; doesn't ask implementors to try anything other than the
;;; specific filename mentioned.  This feature is likely to
;;; be requested, however, so we might as well debug it now.
;;;
;;; Note: no initial period, because the resolver will insert one.

(define *include-suffixes-source*
  '("sch"    ; Larceny's usual extension, can override "scm"
    "scm"    ; de facto standard for R7RS source
    "ss"     ; FIXME: can we drop this one?
    "larceny.scm"))

(define (larceny:include-ci-file filename)
  (parameterize ((case-sensitive? #f))
   (larceny:include-file filename)))

(define (larceny:include-library-declarations-file filename)
  (larceny:include-file filename))

;;; The current-require-path and current-library-resolver are
;;; used to find the file to include.
;;;
;;; FIXME: this should guard against infinite recursive includes.

(define (larceny:include-file filename)
  (let* ((require-paths (current-require-path))
         (fname
          (call-with-current-continuation
           (lambda (return)
             (define (try-path path)

               (begin (display "Trying ")
                      (display path)
                      (display "/")
                      (display filename)
                      (newline))
               (parameterize ((current-require-path (list path))
                              (current-require-path-suffix-optional #t)
                              (current-require-path-suffixes
                               *include-suffixes-source*)
                              (current-require-path-suffixes-compiled '()))
                (let ((fname ((current-library-resolver) filename)))
                  (if fname
                      (return fname)))))
             (if (file-exists? filename)
                 (return filename))
             (for-each try-path require-paths)
             #f))))
    (if fname
        (let* ((dirname (larceny:directory-of fname))
               (basename (larceny:file-name-only fname))
               (forms (larceny:include-file-forms fname)))
          (larceny:make-included-begin-form forms dirname basename))
        (assertion-violation 'include "file not found" filename))))

(define (larceny:include-file-forms filename)
#;(begin (display "Reading ") ; FIXME: temporary aid to debugging
         (display filename)
         (newline))
  (call-with-input-file
   filename
   (lambda (p)
     (let loop ((forms '()))
       (let ((x (read p)))
         (if (eof-object? x)
             (reverse forms)
             (loop (cons x forms))))))))

