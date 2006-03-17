; SRFI 0: Feature-based conditional expansion construct.
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-0/srfi-0.html> for the full document.
;
; Copyright (C) Marc Feeley (1999). All Rights Reserved. 
;
; This document and translations of it may be copied and furnished to
; others, and derivative works that comment on or otherwise explain it or
; assist in its implementation may be prepared, copied, published and
; distributed, in whole or in part, without restriction of any kind,
; provided that the above copyright notice and this paragraph are included
; on all such copies and derivative works.  However, this document itself
; may not be modified in any way, such as by removing the copyright notice
; or references to the Scheme Request For Implementation process or
; editors, except as needed for the purpose of developing SRFIs in which
; case the procedures for copyrights defined in the SRFI process must be
; followed, or as required to translate it into languages other than
; English.

; Add SRFI identifiers to the keyword list as they are implemented, and 
; then add clauses near the end to expand into code.

; Larceny
;
; The commented-out names are SRFIs examined and found to require
; at least some implementation work (ie the reference implementations
; are not R5RS).  Some require a lot.
;
; Contributions of missing SRFIs are welcome.

(define-syntax cond-expand
  (syntax-rules (and or not else 
		 larceny 
		 unix linux macosx solaris win32
		 sparc x86 ppc
		 little-endian big-endian
                 srfi-0                 ; Feature-based conditional expansion
		 srfi-1			; List-processing library
		 srfi-2                 ; AND-LET*
		 ;srfi-4                ; Homogenous vectors
		 srfi-5			; Compatible LET form
                 srfi-6                 ; Basic string ports
		 srfi-7			; Program configuration language
		 srfi-8			; RECEIVE: bind multiple values
		 srfi-9			; Defining record types
                 srfi-11                ; LET-VALUES and LET*-VALUES
		 srfi-13                ; String library
		 srfi-14                ; Char-set library
		 srfi-16		; CASE-LAMBDA
		 srfi-17		; Generalized SET!
		 ;srfi-18               ; Multithreading support
		 srfi-19                ; Time and date datatypes
		 ;srfi-21               ; Real-time multithreading support
		 srfi-22                ; Running scheme scripts on Unix
		 srfi-23		; ERROR
		 srfi-25                ; Multi-dimensional array primitives
		 srfi-26		; CUT and CUTE
		 srfi-27		; Random bits
		 srfi-28		; Basic format strings
		 srfi-29		; Localizations
		 srfi-30		; Nested # |...| # comments
		 srfi-31		; REC
		 ;srfi-34               ; Exception handling
		 ;srfi-35               ; Conditions
		 ;srfi-36               ; I/O conditions
		 srfi-37		; ARGS-FOLD
		 srfi-38		; I/O of shared structure
		 srfi-39		; Parameter objects
		 srfi-42		; Eager comprehensions
                 )
    ((cond-expand) 
     (syntax-error "Unfulfilled cond-expand"))

    ;; Standard cond-expand language
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req1) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand more-clauses ...))
       (else body ...)))

    ;; Extension: recognize LARCENY as a keyword
    ((cond-expand (larceny body ...) more-clauses ...)
     (begin body ...))

    ;; Extension: recognize various operating system designators
    ((cond-expand (unix body ...) more-clauses ...)
     (cond-expand-implementation ("unix" body ...) more-clauses ...))
    ((cond-expand (linux body ...) more-clauses ...)
     (cond-expand-implementation ("linux" body ...) more-clauses ...))
    ((cond-expand (macosx body ...) more-clauses ...)
     (cond-expand-implementation ("macosx" body ...) more-clauses ...))
    ((cond-expand (solaris body ...) more-clauses ...)
     (cond-expand-implementation ("solaris" body ...) more-clauses ...))
    ((cond-expand (win32 body ...) more-clauses ...)
     (cond-expand-implementation ("win32" body ...) more-clauses ...))

    ;; Extension: recognize various architecture designators
    ((cond-expand (sparc body ...) more-clauses ...)
     (cond-expand-implementation ("sparc" body ...) more-clauses ...))
    ((cond-expand (x86 body ...) more-clauses ...)
     (cond-expand-implementation ("x86" body ...) more-clauses ...))
    ((cond-expand (ppc body ...) more-clauses ...)
     (cond-expand-implementation ("ppc" body ...) more-clauses ...))

    ;; Extension: recognize various endianness designators
    ((cond-expand (little-endian body ...) more-clauses ...)
     (cond-expand-implementation ("little-endian" body ...) more-clauses ...))
    ((cond-expand (big-endian body ...) more-clauses ...)
     (cond-expand-implementation ("big-endian" body ...) more-clauses ...))

    ;; Recognize the various SRFIs
    ((cond-expand (srfi-0 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-1 body ...) more-clauses ...)
     (begin (require 'srfi-1) body ...))
    ((cond-expand (srfi-2 body ...) more-clauses ...)
     (begin (require 'srfi-2) body ...))
    ((cond-expand (srfi-5 body ...) more-clauses ...)
     (begin (require 'srfi-5) body ...))
    ((cond-expand (srfi-6 body ...) more-clauses ...)
     (begin (require 'srfi-6) body ...))
    ((cond-expand (srfi-7 body ...) more-clauses ...)
     (begin (require 'srfi-7) body ...))
    ((cond-expand (srfi-8 body ...) more-clauses ...)
     (begin (require 'srfi-8) body ...))
    ((cond-expand (srfi-9 body ...) more-clauses ...)
     (begin (require 'srfi-9) body ...))
    ((cond-expand (srfi-11 body ...) more-clauses ...)
     (begin (require 'srfi-11) body ...))
    ((cond-expand (srfi-13 body ...) more-clauses ...)
     (begin (require 'srfi-13) body ...))
    ((cond-expand (srfi-14 body ...) more-clauses ...)
     (begin (require 'srfi-14) body ...))
    ((cond-expand (srfi-16 body ...) more-clauses ...)
     (begin (require 'srfi-16) body ...))
    ((cond-expand (srfi-17 body ...) more-clauses ...)
     (begin (require 'srfi-17) body ...))
    ((cond-expand (srfi-19 body ...) more-clauses ...)
     (begin (require 'srfi-19) body ...))
    ((cond-expand (srfi-22 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-23 body ...) more-clauses ...)
     (begin (require 'srfi-23) body ...))
    ((cond-expand (srfi-25 body ...) more-clauses ...)
     (begin (require 'srfi-25) body ...))
    ((cond-expand (srfi-26 body ...) more-clauses ...)
     (begin (require 'srfi-26) body ...))
    ((cond-expand (srfi-28 body ...) more-clauses ...)
     (begin (require 'srfi-27) body ...))
    ((cond-expand (srfi-27 body ...) more-clauses ...)
     (begin (require 'srfi-28) body ...))
    ((cond-expand (srfi-29 body ...) more-clauses ...)
     (begin (require 'srfi-29) body ...))
    ((cond-expand (srfi-30 body ...) more-clauses ...)
     (begin (require 'srfi-30) body ...))
    ((cond-expand (srfi-31 body ...) more-clauses ...)
     (begin (require 'srfi-31) body ...))
    ((cond-expand (srfi-37 body ...) more-clauses ...)
     (begin (require 'srfi-37) body ...))
    ((cond-expand (srfi-38 body ...) more-clauses ...)
     (begin (require 'srfi-38) body ...))
    ((cond-expand (srfi-39 body ...) more-clauses ...)
     (begin (require 'srfi-39) body ...))
    ((cond-expand (srfi-42 body ...) more-clauses ...)
     (begin (require 'srfi-42) body ...))

    ;; Other features are not supported!
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

(define-syntax cond-expand-implementation
  (transformer
   (lambda (exp rename compare)
     (let* ((f    (system-features))
	    (arch (cdr (assq 'arch-name f)))
	    (os   (cdr (assq 'os-name f)))
	    (end  (cdr (assq 'arch-endianness f)))
	    (x    (caadr exp)))
       (if (or (and (string=? x "unix")
		    (or (string=? os "SunOS")
			(string=? os "Linux")
			(string=? os "OSF")
			(string=? os "Unix")
			(string=? os "BSD Unix")
			(string=? os "MacOS X")))
	       (and (string=? x "linux")
		    (string=? os "Linux"))
	       (and (string=? x "win32")
		    (string=? os "Win32"))
	       (and (string=? x "macosx")
		    (string=? os "MacOS X"))
	       (and (string=? x "solaris")
		    (string=? os "SunOS"))
	       (and (string=? x "sparc")
		    (string=? arch "SPARC"))
	       (and (string=? x "x86")
		    (string=? arch "X86-NASM"))
	       (and (string=? x "ppc")
		    (string=? arch "PPC"))
	       (and (string=? x "petit")
		    (string=? arch "Standard-C"))
	       (and (string=? x "little-endian")
		    (eq? end 'little))
	       (and (string=? x "big-endian")
		    (eq? end 'big)))
	   `(,(rename 'begin) ,@(cdadr exp))
	   `(,(rename 'cond-expand) ,@(cddr exp)))))))
		  

; Not the best place for this, but oh well

(define-syntax syntax-error
  (transformer
   (lambda (exp rename compare)
     (error "Syntax error: " (if (null? (cdr exp)) "???" (cadr exp))))))

; eof
