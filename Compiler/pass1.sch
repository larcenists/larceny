; Copyright 1991 William Clinger
;
; $Id$
;
; 20 November 1998
;
; First pass of the Twobit compiler:
;   macro expansion, syntax checking, alpha conversion,
;   preliminary annotation.
;
; The input to this pass is a Scheme definition or expression.
; The output is an expression in the subset of Scheme described
; by the following grammar, where the output satisfies certain
; additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the output:
;   *  There are no internal definitions.
;   *  No identifier containing an upper case letter is bound anywhere.
;      (Change the "name:..." variables if upper case is preferred.)
;   *  No identifier is bound in more than one place.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.
;   *  F and G are garbage.

(define source-file-name #f)
(define source-file-position #f)

(define (pass1 def-or-exp . rest)
  (set! source-file-name #f)
  (set! source-file-position #f)
  (if (not (null? rest))
      (begin (set! source-file-name (car rest))
             (if (not (null? (cdr rest)))
                 (set! source-file-position (cadr rest)))))
  (set! renaming-counter 0)
  (macro-expand def-or-exp))
