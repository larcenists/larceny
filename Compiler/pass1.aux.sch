; Copyright 1991 William Clinger
;
; $Id$
;
; 12 December 1998 / wdc

($$trace "pass1.aux")

;***************************************************************
;
; Each definition in this section should be overridden by an assignment
; in a target-specific file.
;
; If a lambda expression has more than @maxargs-with-rest-arg@ required
; arguments followed by a rest argument, then the macro expander will
; rewrite the lambda expression as a lambda expression with only one
; argument (a rest argument) whose body is a LET that binds the arguments
; of the original lambda expression.

(define @maxargs-with-rest-arg@
  1000000)                              ; infinity

(define (prim-entry name) #f)           ; no integrable procedures
(define (prim-arity name) 0)            ; all of which take 0 arguments
(define (prim-opcodename name) name)    ; and go by their source names

; End of definitions to be overridden by target-specific assignments.
;
;***************************************************************

; Miscellaneous routines.

(define (m-warn msg . more)
  (if (issue-warnings)
      (begin
       (display "WARNING from macro expander:")
       (newline)
       (display msg)
       (newline)
       (for-each (lambda (x) (write x) (newline))
                 more))))

(define (m-error msg . more)
  (display "ERROR detected during macro expansion:")
  (newline)
  (display msg)
  (newline)
  (for-each (lambda (x) (write x) (newline))
            more)
  (m-quit (make-constant #f)))

(define (m-bug msg . more)
  (display "BUG in macro expander: ")
  (newline)
  (display msg)
  (newline)
  (for-each (lambda (x) (write x) (newline))
            more)
  (m-quit (make-constant #f)))

; Given a <formals>, returns a list of bound variables.

'
(define (make-null-terminated x)
  (cond ((null? x) '())
        ((pair? x)
         (cons (car x) (make-null-terminated (cdr x))))
        (else (list x))))

; Returns the length of the given list, or -1 if the argument
; is not a list.  Does not check for circular lists.

(define (safe-length x)
  (define (loop x n)
    (cond ((null? x) n)
          ((pair? x) (loop (cdr x) (+ n 1)))
          (else -1)))
  (loop x 0))

; Given a unary predicate and a list, returns a list of those
; elements for which the predicate is true.

(define (filter1 p x)
  (cond ((null? x) '())
        ((p (car x)) (cons (car x) (filter1 p (cdr x))))
        (else (filter1 p (cdr x)))))

; Given a unary predicate and a list, returns #t if the
; predicate is true of every element of the list.

(define (every1? p x)
  (cond ((null? x) #t)
        ((p (car x)) (every1? p (cdr x)))
        (else #f)))

; Binary union of two sets represented as lists, using equal?.

(define (union2 x y)
  (cond ((null? x) y)
        ((member (car x) y)
         (union2 (cdr x) y))
        (else (union2 (cdr x) (cons (car x) y)))))

; Given an association list, copies the association pairs.

(define (copy-alist alist)
  (map (lambda (x) (cons (car x) (cdr x)))
       alist))

; Removes a value from a list.  May destroy the list.

'
(define remq!
  (letrec ((loop (lambda (x y prev)
                   (cond ((null? y) #t)
                         ((eq? x (car y))
                          (set-cdr! prev (cdr y))
                          (loop x (cdr prev) prev))
                         (else
                          (loop x (cdr y) y))))))
    (lambda (x y)
      (cond ((null? y) '())
            ((eq? x (car y))
             (remq! x (cdr y)))
            (else
             (loop x (cdr y) y)
             y)))))

; Procedure-specific source code transformations.
; The transformer is passed a source code expression and a predicate
; and returns one of:
;
;    the original source code expression
;    a new source code expression to use in place of the original
;    #f to indicate that the procedure is being called
;      with an incorrect number of arguments or
;      with an incorrect operand
;
; The original source code expression is guaranteed to be a list whose
; car is the name associated with the transformer.
; The predicate takes an identifier (a symbol) and returns true iff
; that identifier is bound to something other than its global binding.
;
; Since the procedures and their transformations are target-specific,
; they are defined in another file, in the Target subdirectory.

; I think this is now used in only one place, in simplify-if.

(define (integrable? name)
  (and (integrate-usual-procedures)
       (prim-entry name)))

; For testing.

; MAKE-READABLE strips the referencing information
; and replaces (begin I) by I.

(define (make-readable exp)
  (case (car exp)
    ((quote)    exp)
    ((lambda)   `(lambda ,(lambda.args exp)
                         ,@(map (lambda (def)
                                  `(define ,(def.lhs def)
                                           ,(make-readable (def.rhs def))))
                                (lambda.defs exp))
                           ,(make-readable (lambda.body exp))))
    ((set!)     `(set! ,(assignment.lhs exp)
                       ,(make-readable (assignment.rhs exp))))
    ((if)       `(if ,(make-readable (if.test exp))
                     ,(make-readable (if.then exp))
                     ,(make-readable (if.else exp))))
    ((begin)    (if (variable? exp)
                    (variable.name exp)
                    `(begin ,@(map make-readable (begin.exprs exp)))))
    (else       `(,(make-readable (call.proc exp))
                  ,@(map make-readable (call.args exp))))))

; MAKE-UNREADABLE does the reverse.
; It assumes there are no internal definitions.

(define (make-unreadable exp)
  (cond ((symbol? exp) (list 'begin exp))
        ((pair? exp)
         (case (car exp)
           ((quote) exp)
           ((lambda) (list 'lambda
                           (cadr exp)
                           '(begin)
                           (list '() '() '() '())
                           (make-unreadable (cons 'begin (cddr exp)))))
           ((set!) (list 'set! (cadr exp) (make-unreadable (caddr exp))))
           ((if) (list 'if
                       (make-unreadable (cadr exp))
                       (make-unreadable (caddr exp))
                       (if (= (length exp) 3)
                           '(unspecified)
                           (make-unreadable (cadddr exp)))))
           ((begin) (if (= (length exp) 2)
                        (make-unreadable (cadr exp))
                        (cons 'begin (map make-unreadable (cdr exp)))))
           (else (map make-unreadable exp))))
        (else (list 'quote exp))))
