; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 13 April 1999.
;
; The third "pass" of the Twobit compiler actually consists of several
; passes, which are related by the common theme of flow analysis:
;   interprocedural inlining of known local procedures
;   interprocedural constant propagation and folding
;   intraprocedural commoning, copy propagation, and dead code elimination
;   representation inference (not yet implemented)
;   register targeting
;
; This pass operates as source-to-source transformations on
; expressions written in the subset of Scheme described by the
; following grammar:
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
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
; Invariants that hold for the input only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R is garbage.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  The expression does not share structure with the original input,
;      but might share structure with itself.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R is garbage.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;
; The phases of pass 3 interact with the referencing information R
; and the free variables F as follows:
;
; Inlining               ignores R,   ignores F,  destroys R,  destroys F.
; Constant propagation      uses R,   ignores F, preserves R, preserves F.
; Conversion to ANF      ignores R,   ignores F,  destroys R,  destroys F.
; Commoning                 uses R,      uses F,  destroys R,  computes F.
; Register targeting        uses R,      uses F,  destroys R,  computes F.

; Common subexpression elimination is turned off because of a bug
; that shows up when compiling Compiler/expand.sch.

(define (pass3 exp)
  
  (define (phase1 exp)
    (if (interprocedural-inlining)
        (let ((g (callgraph exp)))
          (inline-using-callgraph! g)
          exp)
        exp))
  
  (define (phase2 exp)
    (if (interprocedural-constant-propagation)
        (constant-propagation (copy-exp exp))
        exp))
  
  (define (phase3 exp)
    (if (common-subexpression-elimination)
        (begin
         '(copy-exp
           (intraprocedural-commoning
            (copy-exp
             (intraprocedural-commoning
              (copy-exp (a-normal-form exp))
              'commoning))
            'target-registers))
         (intraprocedural-commoning (copy-exp (a-normal-form exp))))
        exp))
  
  (define (phase4 exp)
    exp)
  
  (define (finish exp)
    (if (and (interprocedural-inlining)
             (not (interprocedural-constant-propagation))
             (not (common-subexpression-elimination)))
        (copy-exp exp)
        exp))
  
  (define (verify exp)
    (check-referencing-invariants exp 'free)
    exp)
  
  (if (global-optimization)
      (verify (finish (phase4 (phase3 (phase2 (phase1 exp))))))
      exp))
