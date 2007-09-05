; Copyright 1992 William Clinger
;
; $Id$
;
; 13 December 1998
; Implementation-dependent parameters and preferences that determine
; how identifiers are represented in the output of the macro expander.
;
; The basic problem is that there are no reserved words, so the
; syntactic keywords of core Scheme that are used to express the
; output need to be represented by data that cannot appear in the
; input.  This file defines those data.

($$trace "prefs")

; FIXME: The following definitions are currently ignored.

; The following definitions assume that identifiers of mixed case
; cannot appear in the input.

(define begin1  (string->symbol "Begin"))
(define define1 (string->symbol "Define"))
(define quote1  (string->symbol "Quote"))
(define lambda1 (string->symbol "Lambda"))
(define if1     (string->symbol "If"))
(define set!1   (string->symbol "Set!"))

; The following defines an implementation-dependent expression
; that evaluates to an undefined (not unspecified!) value, for
; use in expanding the (define x) syntax.

(define undefined1 (list (string->symbol "Undefined")))

; End of FIXME.

; A variable is renamed by suffixing a vertical bar followed by a unique
; integer.  In IEEE and R4RS Scheme, a vertical bar cannot appear as part
; of an identifier, but presumably this is enforced by the reader and not
; by the compiler.  Any other character that cannot appear as part of an
; identifier may be used instead of the vertical bar.

(define renaming-prefix-character #\.)
(define renaming-suffix-character #\|)

(define renaming-prefix (string renaming-prefix-character))
(define renaming-suffix (string renaming-suffix-character))

; Patches for Twobit.  Here temporarily.

(define (make-toplevel-definition id exp)
  (if (lambda? exp)
      (doc.name-set! (lambda.doc exp) (m-strip id)))
  (make-begin
   (list (make-assignment id exp)
         (make-constant id))))
        
(define (make-undefined)
  (make-call (make-variable '.undefined) '()))

(define (make-unspecified)
  (make-call (make-variable '.unspecified) '()))
