; Copyright 1991 William Clinger
;
; $Id$
;
; 24 April 1999
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

($$trace "pass1")

(define source-file-name #f)
(define source-file-position #f)

(define pass1-block-compiling? #f)
(define pass1-block-assignments '())
(define pass1-block-inlines '())

; pass1 and pass1-block take a syntax environment and an optional inline
; environment, which is just a second (but immutable) syntax environment.

(define (pass1 def-or-exp syntaxenv . rest)
  (set! source-file-name #f)
  (set! source-file-position #f)
  (set! pass1-block-compiling? #f)
  (set! pass1-block-assignments '())
  (set! pass1-block-inlines '())
  (set! renaming-counter 0)
  (apply twobit-expand def-or-exp syntaxenv rest))

; Compiles a whole sequence of top-level forms on the assumption
; that no variable that is defined by a form in the sequence is
; ever defined or assigned outside of the sequence.
;
; This is a crock in three parts:
;
;    1.  Twobit-expand each form and record assignments.
;    2.  Find the top-level variables that are defined but not
;        assigned, give them local names, generate a DEFINE-INLINE
;        for each of the top-level procedures, and twobit-expand
;        each form again.
;    3.  Wrap the whole mess in an appropriate LET and recompute
;        the referencing information by copying it.
;
; Note that macros get expanded twice, and that all DEFINE-SYNTAX
; macros are considered local to the forms.

; FIXME: Need to turn off warning messages.

(define (pass1-block forms syntaxenv . rest)
  
  (define (part1)
    (set! pass1-block-compiling? #t)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (set! renaming-counter 0)
    (let ((env0 (syntactic-copy (global-syntactic-environment)))
          (bmode (benchmark-mode))
          (wmode (issue-warnings))
          (defined '()))
      (define (make-toplevel-definition id exp)
        (cond ((memq id defined)
               (set! pass1-block-assignments
                     (cons id pass1-block-assignments)))
              ((or (constant? exp)
                   (and (lambda? exp)
                        (list? (lambda.args exp))))
               (set! defined (cons id defined))))
        (make-begin
         (list (make-assignment id exp)
               (make-constant id))))
      (benchmark-mode #f)
      (issue-warnings #f)
      (for-each (lambda (form)
                  (desugar-definitions form
                                       (global-syntactic-environment)
                                       make-toplevel-definition))
                forms)
      (global-syntactic-environment env0)
      (benchmark-mode bmode)
      (issue-warnings wmode)
      (part2 (filter (lambda (id)
                       (not (memq id pass1-block-assignments)))
                     (reverse defined)))))
  
  (define (part2 defined)
    (set! pass1-block-compiling? #f)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (set! renaming-counter 0)
    (let* ((rename (make-rename-procedure))
           (alist (map (lambda (id)
                         (cons id (rename id)))
                       defined))
           (definitions0 '())    ; for constants
           (definitions1 '()))   ; for lambda expressions
      (define (make-toplevel-definition id exp)
        (if (lambda? exp)
            (doc.name-set! (lambda.doc exp) (m-strip id)))
        (let ((probe (assq id alist)))
          (if probe
              (let ((id1 (cdr probe)))
                (cond ((constant? exp)
                       (set! definitions0
                             (cons (make-assignment id exp)
                                   definitions0))
                       (make-constant id))
                      ((lambda? exp)
                       (set! definitions1
                             (cons (make-assignment id1 exp)
                                   definitions1))
                       (make-assignment
                        id
                        (make-lambda (lambda.args exp)
                                     '() ; no definitions
                                     '() ; R
                                     '() ; F
                                     '() ; G
                                     '() ; decls
                                     (lambda.doc exp)
                                     (make-call
                                      (make-variable id1)
                                      (map make-variable
                                           (lambda.args exp))))))
                      (else
                       (m-error "Inconsistent macro expansion"
                                (make-readable exp)))))
              (make-assignment id exp))))
      (let ((env0 (syntactic-copy (global-syntactic-environment)))
            (bmode (benchmark-mode))
            (wmode (issue-warnings)))
        (issue-warnings #f)
        (for-each (lambda (pair)
                    (let ((id0 (car pair))
                          (id1 (cdr pair)))
                      (syntactic-bind-globally!
                       id0
                       (make-inline-denotation
                        id0
                        (lambda (exp rename compare)
                          ; Deliberately non-hygienic!
                          (cons id1 (cdr exp)))
                        (global-syntactic-environment)))
                      (set! pass1-block-inlines
                            (cons id0 pass1-block-inlines))))
                  alist)
        (benchmark-mode #f)
        (issue-warnings wmode)
        (let ((forms
               (do ((forms forms (cdr forms))
                    (newforms '()
                              (cons (desugar-definitions
                                     (car forms)
                                     (global-syntactic-environment)
                                     make-toplevel-definition)
                                    newforms)))
                   ((null? forms)
                    (reverse newforms)))))
          (benchmark-mode bmode)
          (global-syntactic-environment env0)
          (part3 alist definitions0 definitions1 forms)))))
  
  (define (part3 alist definitions0 definitions1 forms)
    (set! pass1-block-compiling? #f)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (let* ((constnames0 (map assignment.lhs definitions0))
           (constnames1 (map (lambda (id0)
                               (cdr (assq id0 alist)))
                             constnames0))
           (procnames1 (map assignment.lhs definitions1)))
      (copy-exp
       (make-call
        (make-lambda
         constnames1
         '() ; no definitions
         '() ; R
         '() ; F
         '() ; G
         '() ; decls
         #f  ; doc
         (make-begin
          (list
           (make-begin
            (cons (make-constant #f)
                  (reverse
                   (map (lambda (id)
                          (make-assignment id (make-variable (cdr (assq id alist)))))
                        constnames0))))
           (make-call
            (make-lambda
             constnames0
             '() ; no definitions
             '() ; R
             '() ; F
             '() ; G
             '() ; decls
             #f  ; doc
             (make-call
              (make-lambda
               (map assignment.lhs definitions1)
               '() ; no definitions
               '() ; R
               '() ; F
               '() ; G
               '() ; decls
               #f  ; doc
               (make-begin (cons (make-constant #f)
                                 (append definitions1 forms))))
              (map (lambda (ignored) (make-unspecified))
                   definitions1)))
            (map make-variable constnames1))
           )))
        (map assignment.rhs definitions0)))))
  
  (error "pass1-block must be modified to deal with the new regime for syntax environments.")
  (set! source-file-name #f)
  (set! source-file-position #f)
  (part1))
