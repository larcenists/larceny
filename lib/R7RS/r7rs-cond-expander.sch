;;; Copyright 2015 William D Clinger
;;;
;;; $Id$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements cond-expand for R7RS define-library.
;;;
;;; The code in this file assumes lib/R6RS/r6rsmode.sch has
;;; been loaded.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a cond-expand form in which the <feature requirement>
;;; part of each clause is a raw s-expression but the
;;; expressions or declarations are in a form that should not
;;; be examined by this code, returns a list of the selected
;;; expressions or declarations.
;;;
;;; If no <feature requirements> evaluates to true, returns an
;;; empty list.

(define (larceny:cond-expand form)
  (larceny:interpret-cond-expand (cdr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private to this file.
;;;
;;; From R7RS section 4.2.1:
;;;
;;; A <feature requirement> takes one of the following forms:
;;;
;;;     <feature identifier>
;;;     (library <library name>)
;;;     (and <feature requirement> ...)
;;;     (or <feature requirement> ...)
;;;     (not <feature requirement>)
;;;
;;; R7RS Appendix B lists these standard feature identifiers:
;;;
;;;     r7rs
;;;     exact-closed
;;;     exact-complex
;;;     ieee-float
;;;     full-unicode
;;;     ratios
;;;     posix
;;;     windows
;;;     unix, darwin, gnu-linux, bsd, freebsd, solaris, ...
;;;     i386, x86-64, ppc, sparc, jvm, clr, llvm, ...
;;;     ilp32, lp64, ilp64, ...
;;;     big-endian, little-endian
;;;     <name> of this implementation, e.g. fantastic-scheme
;;;     <name-version> of this implementation, e.g. fantastic-scheme-1.0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (larceny:get-feature name)
  (let ((probe (assq name (system-features))))
    (and probe (cdr probe))))

;;; FIXME: keep this in sync with src/Lib/Common/system-interface.sch

(define (larceny:evaluate-feature feature)
  (define (complain)
    (assertion-violation 'cond-expand "malformed feature requirement" feature))
  (cond ((symbol? feature)
         (case feature
          ((r7rs)
           ;; Should ERR5RS mode count as R7RS?
           (memq (larceny:get-feature 'execution-mode)
                 '(r7rs err5rs)))
          ((larceny exact-closed exact-complex ieee-float ratios)
           #t)
          ((larceny-0.98 larceny-0.99 larceny-2.0)
           ;; FIXME: should strip off trailing beta version, etc
           (let* ((major (larceny:get-feature 'larceny-major-version))
                  (minor (larceny:get-feature 'larceny-minor-version))
                  (s (string-append "larceny-"
                                    (number->string major)
                                    "."
                                    (number->string minor))))
             (string=? (symbol->string feature) s)))
          ((full-unicode)
           ;; Larceny can always represent every Unicode character,
           ;; but Unicode strings are a different story.
           (eq? (larceny:get-feature 'char-representation) 'unicode))
          ((posix)
           (member (larceny:get-feature 'os-name)
                   '("SunOS"
                     "Linux"    ; close enough
                     "MacOS X"  ; close enough
                     )))
          ((windows)
           (eq? (larceny:os) 'windows))
          ((unix)
           (eq? (larceny:os) 'unix))
          ((darwin)
           (string=? (larceny:get-feature 'os-name) "MacOS X"))
          ((gnu-linux)
           (string=? (larceny:get-feature 'os-name) "Linux"))
          ((i386 ilp32)
           (member (larceny:get-feature 'arch-name)
                   '("X86-NASM" "IAssassin")))
          ((ppc)
           ;; Petit Larceny runs on the PowerPC but doesn't know it.
           #f)
          ((arm)
           (string=? (larceny:get-feature 'arch-name)
                     "ARM"))
          ((sparc)
           (string=? (larceny:get-feature 'arch-name)
                     "SPARC"))
          ((clr)
           (string=? (larceny:get-feature 'arch-name)
                     "CLR"))
          ((llvm lp64 ilp64)
           ;; Sadly.
           #f)
          ((big-endian)
           (eq? (larceny:get-feature 'arch-endianness) 'big))
          ((little-endian)
           (eq? (larceny:get-feature 'arch-endianness) 'little))
          (else #f)))
        ((pair? feature)
         (case (car feature)
          ((library)
           (cond ((or (null? (cdr feature))
                      (not (null? (cddr feature))))
                  (complain))
                 ((not (list? (cadr feature)))
                  (complain))
                 (else
                  (let* ((libname (cadr feature))
                         (fname (larceny:find-r6rs-library (cadr feature))))
                    (and fname
                         (larceny:find-r6rs-library-really? libname fname))))))
          ((and)
           (if (null? (cdr feature))
               #t
               (and (larceny:evaluate-feature (car feature))
                    (larceny:evaluate-feature `(and ,@(cdr feature))))))
          ((or)
           (if (null? (cdr feature))
               #f
               (or (larceny:evaluate-feature (car feature))
                   (larceny:evaluate-feature `(and ,@(cdr feature))))))
          ((not)
           (cond ((or (null? (cdr feature))
                      (not (null? (cddr feature))))
                  (complain))
                 (else
                  (not (larceny:evaluate-feature (car feature))))))
          (else (complain))))
        (else (complain))))

(define (larceny:interpret-cond-expand clauses)

  ;; FIXME: the form is partly syntax, so this won't do much good.

  (define (complain)
    (assertion-violation 'cond-expand "malformed cond-expand"
                         (cons 'cond-expand clauses)))

  (let loop ((clauses clauses))
    (cond ((null? clauses)
           '())
          ((or (not (list? (car clauses)))
               (not (pair? (car clauses))))
           (complain))
          ((larceny:evaluate-feature (caar clauses))
           (cdr (car clauses)))
          (else
           (loop (cdr clauses))))))
