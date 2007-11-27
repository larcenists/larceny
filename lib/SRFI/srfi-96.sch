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

(define (program-arguments)
  (cons "larceny" (vector->list (command-line-arguments))))

; FIXME:  This doesn't work with Petit Larceny or Common Larceny
; or Windows.

(define (current-time)
  (if (and (eq? (software-type) 'unix)
           (eq? (scheme-implementation-type) 'larceny))
      (let ((bv (make-bytevector 8)))

        ; Returns the number of seconds since Jan 1, 1970 00:00:00 GMT.
        ; The argument should be #f or a bytevector of length  
        ; at least 4, in which to store the time.  See time(2).

        (define unix:time
          (let ((_time (foreign-procedure "time" '(boxed) 'int)))
            (lambda (arg)
              (if (and arg
                       (not (and (bytevector? arg)
                                 (>= (bytevector-length arg) 4))))
                  (error "Invalid parameter to unix:time"))
              (_time arg))))

        (unix:time #f))

      0))

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

; FIXME:  This should probably be ".sch", but then SLIB wouldn't work.

(define (scheme-file-suffix) ".scm")

;;; SLIB:FEATURES is a list of symbols naming the (SLIB) features
;;; initially supported by this implementation.

(define slib:features
      `(
        source                          ;can load scheme source files
                                        ;(SLIB:LOAD-SOURCE "filename")
        compiled                        ;can load compiled files
                                        ;(SLIB:LOAD-COMPILED "filename")
        vicinity

        srfi-59

;;;     char-ready?

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

        ,@(if (positive? (current-time))
              '(current-time)           ;returns time in seconds since 1/1/1970
              '())

        ;; Scheme report features
        ;; R5RS-compliant implementations should provide all 9 features.

        r5rs                            ;conforms to

        eval                            ;R5RS two-argument eval

        values                          ;R5RS multiple values

        dynamic-wind                    ;R5RS dynamic-wind

        macro                           ;R5RS high level macros

        delay                           ;has DELAY and FORCE

        multiarg-apply                  ;APPLY can take more than 2 args.

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
                                        ;SLIB's redefinition of require will
                                        ;break srfi-0, cond-expand

        defmacro                        ;has Common Lisp DEFMACRO

        record                          ;has user defined data structures
                                        ;FIXME: ERR5RS/R6RS procedural API

        string-port                     ;has CALL-WITH-INPUT-STRING and
                                        ;CALL-WITH-OUTPUT-STRING

;;;     sort

        pretty-print

;;;     object->string

;;;     format                          ;Common-lisp output formatting

        trace                           ;has macros: TRACE and UNTRACE
                                        ;(FIXME: as procedures, not macros)

        compiler                        ;has (COMPILER)
                                        ;FIXME: via eval, compile-file

        ;; Implementation Specific features

        ))

;;; most-positive-fixnum is used in modular.scm

(define most-positive-fixnum (expt 2 (- 24 2)))

;;; char-code-limit is one greater than the largest integer which can
;;; be returned by char->integer.

(define char-code-limit #x110000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File-System
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define with-load-pathname
  (let ((exchange
         (lambda (new)
           (let ((old *load-pathname*))
             (set! *load-pathname* new)
             old))))
    (lambda (path thunk)
      (let* ((old (exchange path))
             (val (thunk)))
        (exchange old)
        val))))

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
;;; FIXME:  I don't see how this could possibly work with
;;; Larceny's defmacro.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Private stuff.

(define (base:eval . args)
  (apply slib:eval args))

;(define (defmacro:expand* x)
;  (require 'defmacroexpand) (apply defmacro:expand* x '()))

(define (defmacro:expand* x)
  (macro-expand x))

(define *defmacros*
  (list (cons 'defmacro
              (lambda (name parms . body)
                `(set! *defmacros* (cons (cons ',name (lambda ,parms ,@body))
                                      *defmacros*))))))

;;; FIXME:
;;; SLIB appears to need these things, even though SRFI 96 doesn't
;;; specify them or indicate that implementations of SRFI 96 must
;;; define them.

(define (macro:eval . args) (apply slib:eval args))

(define (macro:load . args) (apply load args))

;;; Public stuff.

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "slib:G" (number->string *gensym-counter*))))))

(define (defmacro? m) (and (assq m *defmacros*) #t))

(define (defmacro:eval x) (base:eval (defmacro:expand* x)))

(define (defmacro:load <pathname>)
  (slib:eval-load <pathname> defmacro:eval))

(define (macroexpand-1 e)
  (if (pair? e)
      (let ((a (car e)))
        (cond ((symbol? a)
               (set! a (assq a *defmacros*))
               (if a (apply (cdr a) (cdr e)) e))
              (else e)))
      e))

(define (macroexpand e)
  (if (pair? e)
      (let ((a (car e)))
        (cond ((symbol? a)
               (set! a (assq a *defmacros*))
               (if a (macroexpand (apply (cdr a) (cdr e))) e))
              (else e)))
      e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (slib:load-source "foo") should load "foo.scm" or with whatever
;;; suffix all the module files in SLIB have.  See feature 'source.

;(define slib:load-source load)

(define (slib:load-source f)
  (load (string-append f (scheme-file-suffix))))

;;; (SLIB:LOAD-COMPILED "foo") should load the file that was produced
;;; by compiling "foo.scm" if this implementation can compile files.
;;; See feature 'COMPILED.

(define (slib:load-compiled f)
  (load (string-append f ".fasl")))

;;; At this point SLIB:LOAD must be able to load SLIB files.

(define slib:load slib:load-source)

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

; FIXME

(define (browse-url url)
  (slib:warn "define BROWSE-URL in larceny.init")
  #f)

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

(slib:load (in-vicinity (library-vicinity) "require"))

; eof
