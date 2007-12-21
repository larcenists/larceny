;;; Copyright 2007 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code in this file is derived from code that was placed
;;; in the public domain by Aubrey Jaffer.  William D Clinger
;;; rewrote it for Larceny.  This file follows Larceny's coding
;;; guidelines, not SLIB's, because it is maintained by Larceny's
;;; implementors as part of Larceny's code base.
;;;
;;; SRFI 96 (and SLIB in general) is deprecated in Larceny
;;; because SLIB redefines require.  That redefinition makes
;;; SLIB incompatible with Larceny's support for other SRFIs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$
;;;
;;; SRFI 96: SLIB Prerequisites
;;;
;;; See <http://srfi.schemers.org/srfi-96/>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Larceny-specific initialization.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'srfi-0)     ; cond-expand

(require 'srfi-59)    ; Vicinity

(require 'defmacro)   ; defmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (software-type) should be set to the generic operating system type.
;;; UNIX, VMS, MACOS, AMIGA and MS-DOS are supported.
;;;
;;; Linux counts as 'unix, so Darwin probably does too.

(define (software-type)
  (let ((os (cdr (assq 'os-name (system-features)))))
    (cond ((string=? os "Win32")
           'ms-dos)
          (else 'unix))))

;;; (scheme-implementation-type) should return the name of the scheme
;;; implementation loading this file.

(define (scheme-implementation-type)
  (let ((arch (cdr (assq 'arch-name (system-features)))))
    (cond ((string=? arch "Standard-C")
           'petit-larceny)
          ((string=? arch "CLR")
           'common-larceny)
          (else
           'larceny))))

;;; (scheme-implementation-version) should return a string describing
;;; the version the scheme implementation loading this file.

(define (scheme-implementation-version)
  (let ((features (system-features)))
    (string-append (number->string
                    (cdr (assq 'larceny-major-version features)))
                   "."
                   (number->string
                    (cdr (assq 'larceny-minor-version features))))))

;;; (scheme-implementation-home-page) should return a (string) URI
;;; (Uniform Resource Identifier) for this scheme implementation's home
;;; page; or false if there isn't one.

(define (scheme-implementation-home-page)
  "http://larceny.ccs.neu.edu/")

; FIXME:  This should really be ".sch", but then SLIB wouldn't work.

(define (scheme-file-suffix) ".scm")

;;; SLIB:FEATURES is a list of symbols naming the (SLIB) features
;;; initially supported by this implementation.

(define slib:features
      '(
        vicinity

        srfi-59

        source                          ;can load scheme source files
                                        ;(SLIB:LOAD-SOURCE "filename")
        compiled                        ;can load compiled files
                                        ;(SLIB:LOAD-COMPILED "filename")
;;;     object-hash                     ;has OBJECT-HASH and OBJECT-UNHASH

        full-continuation               ;can return multiple times

        ieee-floating-point             ;conforms to IEEE Standard 754-1985
                                        ;IEEE Standard for Binary
                                        ;Floating-Point Arithmetic.

;;;     sicp                            ;runs code from Structure and
                                        ;Interpretation of Computer
                                        ;Programs by Abelson and Sussman.

;;;     ed                              ;(ED) is editor

        system                          ;posix (system <string>)

        getenv                          ;posix (getenv <string>)

        program-arguments               ;returns list of strings (argv)

        current-time                    ;returns time in seconds since 1/1/1970

        ;; Scheme report features
        ;; R5RS-compliant implementations should provide all 9 features.

        r5rs                            ;conforms to

        eval                            ;R5RS two-argument eval

        values                          ;R5RS multiple values

        dynamic-wind                    ;R5RS dynamic-wind

        macro                           ;R5RS high level macros

        delay                           ;has DELAY and FORCE

        multiarg-apply                  ;APPLY can take more than 2 args.

        char-ready?                     ;has char-ready?

        rev4-optional-procedures        ;LIST-TAIL, STRING-COPY,
                                        ;STRING-FILL!, and VECTOR-FILL!

        ;; These four features are optional in both R4RS and R5RS

        multiarg/and-                   ;/ and - can take more than 2 args.

        rationalize

;;;     transcript                      ;TRANSCRIPT-ON and TRANSCRIPT-OFF

        with-file                       ;has WITH-INPUT-FROM-FILE and
                                        ;WITH-OUTPUT-TO-FILE

        ieee-p1178                      ;conforms to

;;;     r4rs                            ;conforms to (FIXME: not sure)

;;;     r3rs                            ;conforms to (FIXME: not sure)

;;;     rev2-procedures                 ;SUBSTRING-MOVE-LEFT!,
                                        ;SUBSTRING-MOVE-RIGHT!,
                                        ;SUBSTRING-FILL!,
                                        ;STRING-NULL?, APPEND!, 1+,
                                        ;-1+, <?, <=?, =?, >?, >=?

        ;; FIXME:  The following features are not described by SRFI 96.

        ;; Other common features

;;;     srfi                            ;srfi-0, COND-EXPAND finds all srfi-*
                                        ;
                                        ;not supported in Larceny because
                                        ;SLIB's redefinition of require
                                        ;will break srfi-0 and cond-expand

        defmacro                        ;has Common Lisp DEFMACRO

        record                          ;has user defined data structures
                                        ;FIXME: ERR5RS/R6RS procedural API

        string-port                     ;has CALL-WITH-INPUT-STRING and
                                        ;CALL-WITH-OUTPUT-STRING

;;;     sort

;;;     pretty-print

;;;     object->string

;;;     format                          ;Common-lisp output formatting

;;;     trace                           ;has macros: TRACE and UNTRACE

        ;; Implementation Specific features

        ))

;;; Implements SLIB's record package.

(define record-modifier record-mutator)

;;; Implements SLIB's current-time package.

(define (current-time)
  (current-seconds))

(define (difftime t1 t0) (- t1 t0))

(define (offset-time t1 offset) (+ t1 offset))

;;; most-positive-fixnum is used in modular.scm
;;;
;;; SRFI 96 says it must be within the range of exact integers
;;; that may result from computing the length of a list, vector,
;;; or string.

(define most-positive-fixnum (- (expt 2 (- 24 2)) 3))

;;; char-code-limit is one greater than the largest integer which can
;;; be returned by char->integer.

(define char-code-limit #x110000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File-System
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; with-load-pathname is defined in srfi-59.sch

;;; (tmpnam) makes a temporary file name.

(define tmpnam
  (let ((cntr 100))
    (lambda ()
      (set! cntr (+ 1 cntr))
      (string-append "slib_" (number->string cntr)))))

;;; file-exists? is predefined by Larceny

;;; delete-file is predefined by Larceny

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input/Output
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-file filename modes)
  (case modes
    ((r)    (open-input-file filename))
    ((rb)   (open-file-input-port filename))
    ((w)    (open-output-file filename))
    ((wb)   (open-file-output-port filename))
    (else (slib:error 'open-file 'mode? modes))))

;;; port? is predefined by Larceny

;;; close-port is predefined by Larceny

(define (call-with-open-ports proc0 . ports0)
  (define proc (car ports))
  (cond ((procedure? proc) (set! ports (cdr ports)))
        (else (set! ports (reverse ports))
              (set! proc (car ports))
              (set! ports (reverse (cdr ports)))))
  (let ((ans (apply proc ports)))
    (for-each close-port ports)
    ans))

;;; current-error-port is predefined by Larceny

;;; force-output flushes any pending output on optional arg output port

(define (force-output . args)
  (if (null? args)
      (flush-output-port (current-output-port))
      (for-each flush-output-port args)))

;;; (file-position <port> . <k>)

(define (file-position . args)
  (define (complain)
    (assertion-violation 'file-position "illegal arguments" args))
  (cond ((null? args)
         (complain))
        ((null? (cdr args))
         (let ((p (car args)))
           (if (port-has-port-position? p)
               (port-position p)
               #f)))
        ((null? (cddr args))
         (let ((p (car args))
               (k (cadr args)))
           (if (port-has-set-port-position!? p)
               (set-port-position! p k))
           #f))
        (else
         (complain))))

;;; (output-port-width <port>)

(define (output-port-width . arg) 79)

;;; (output-port-height <port>)

(define (output-port-height . arg) 24)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defmacro
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME:
;;; SLIB appears to need these things, even though SRFI 96 doesn't
;;; specify them or indicate that implementations of SRFI 96 must
;;; define them.

(define (macro:eval . args) (apply slib:eval args))

(define (macro:load . args) (apply load args))

;;;

;;; Public stuff.

;;; defmacro is defined by Larceny's defmacro package

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "slib:G" (number->string *gensym-counter*))))))

;;; FIXME:  With Larceny's implementation of defmacro,
;;; macros that have been defined using defmacro are
;;; indistinguishable from macros that have been defined
;;; using Larceny's low-level explicit-renaming facility.
;;;
;;; For SLIB, however, it's probably good enough to pretend
;;; that all low-level macros were defined using defmacro.
;;;
;;; FIXME:  This is terribly representation-dependent,
;;; and will break when (not if) the representation of
;;; macros changes.

(define (defmacro? m)
  (let ((x (environment-get-macro (interaction-environment) m)))
    (and x (procedure? (cadr x)))))

(define (defmacro:eval x) (slib:eval (defmacro:expand* x)))

;;; FIXME:  The specification of defmacro:eval says it
;;; has to use slib:eval, but the definition of
;;; defmacro:load says no such thing.

(define defmacro:load load)

;;; FIXME:  There doesn't seem to be any analogue of
;;; macroexpand-1 in Larceny.

(define (macroexpand-1 e) (macro-expand e))

(define macroexpand macro-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; R5RS Macros
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define macro:expand macro-expand)

(define (macro:eval exp)
  (eval exp (interaction-environment)))

(define macro:load load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define slib:load-source load)

(define slib:load-compiled load)

(define slib:load load)

;;; SLIB:EVAL is single argument eval using the top-level (user) environment.

(define (slib:eval x)
  (eval x (interaction-environment)))

(define (slib:eval-load filename eval)
  (if (file-exists? filename)
      (call-with-input-file
       filename
       (lambda (p)
         (do ((x (read p) (read p)))
             ((eof-object? x))
           (eval x))))))

(define slib:warn
  (lambda args
    (let ((cep (current-error-port)))
      (display "SLIB:warn: " cep)
      (for-each (lambda (x) (display #\space cep) (write x cep)) args))))

(define slib:error
  (lambda args
    (error #f "SLIB:error:" args)))

(define slib:exit
  (lambda args
    (apply exit args)))

;;; FIXME

(define (browse-url url)
  (slib:warn "Larceny provides no browser")
  #f)

;;; getenv and system are predefined by Larceny.

(define (program-arguments)
  (cons "larceny" (vector->list (command-line-arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellany
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identity x) x)

(define slib:tab #\tab)

(define slib:form-feed #\page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mutual Exclusion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-exchanger obj)
  (let ((v (vector obj)))
    (define (exchange new)
      (let* ((old0 (vector-ref v 0))
             (old1 (vector-like-cas! v 0 old0 new)))
        (if (eq? old0 old1)
            old0
            (exchange new))))
    (lambda (new) (exchange new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Legacy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t #t)

(define nil #f)

;;; last-pair is predefined by Larceny

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Deployment
;;;
;;; In Larceny, the SLIB distribution usually goes in lib/SLIB.
;;; If it is placed somewhere else, then the location of SLIB
;;; in the startup.sch file should be edited.  Alternatively,
;;; users of SLIB could just use the -path command-line option
;;; to specify the location of SLIB.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: the following line redefines Larceny's require
;;; procedure, which breaks Larceny's support for SRFIs,
;;; cond-expand, ERR5RS, R6RS, and a lot of other things.
;;;
;;; Workaround:  Don't require srfi-96 until all other
;;; libraries that your program needs have been required.

(slib:load (in-vicinity (library-vicinity) "require"))

; eof
