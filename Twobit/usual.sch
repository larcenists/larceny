; Copyright 1992 William Clinger
;
; $Id$
;
; 14 September 2000.

($$trace "usual")

; The usual macros, adapted from Jonathan's Version 2 implementation.
; DEFINE is handled primitively, since top-level DEFINE has a side
; effect on the global syntactic environment, and internal definitions
; have to be handled specially anyway.
;
; Some extensions are noted, as are some optimizations.
;
; The LETREC* scope rule is used here to protect these macros against
; redefinition of LAMBDA etc.  The scope rule is changed to LETREC at
; the end of this file.

(define-syntax-scope 'letrec*)

(for-each (lambda (form)
            (macro-expand form usual-syntactic-environment))
          '(

; Named LET is defined later, after LETREC has been defined.

(define-syntax let
  (syntax-rules ()
    ((let ((?name ?val) ...) ?body ?body1 ...)
     ((lambda (?name ...) ?body ?body1 ...) ?val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () ?body ?body1 ...)
     (let () ?body ?body1 ...))
    ((let* ((?name1 ?val1) (?name ?val) ...) ?body ?body1 ...)
     (let ((?name1 ?val1)) (let* ((?name ?val) ...) ?body ?body1 ...)))))

; Internal definitions have to be handled specially anyway,
; so we might as well rely on them here.

(define-syntax letrec
  (syntax-rules (lambda quote)
   ((letrec ((?name ?val) ...) ?body ?body2 ...)
    ((lambda ()
       (define ?name ?val) ...
       ?body ?body2 ...)))))

; This definition of named LET extends the prior definition of LET.
; The first rule is non-circular, thanks to the LET* scope that is
; specified for this use of DEFINE-SYNTAX.

(define-syntax let let*
  (syntax-rules ()
    ((let (?bindings ...) . ?body)
     (let (?bindings ...) . ?body))
    ((let ?tag ((?name ?val) ...) ?body ?body1 ...)
     (let ((?name ?val) ...)
       (letrec ((?tag (lambda (?name ...) ?body ?body1 ...)))
         (?tag ?name ...))))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and ?e) ?e)
    ((and ?e1 ?e2 ?e3 ...)
     (if ?e1 (and ?e2 ?e3 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or ?e) ?e)
    ((or ?e1 ?e2 ?e3 ...)
     (let ((temp ?e1))
       (if temp temp (or ?e2 ?e3 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else ?result ?result2 ...))
     (begin ?result ?result2 ...))
    
    ((cond (?test => ?result))
     (let ((temp ?test))
       (if temp (?result temp))))
    
    ((cond (?test)) ?test)
    
    ((cond (?test ?result ?result2 ...))
     (if ?test (begin ?result ?result2 ...)))
    
    ((cond (?test => ?result) ?clause ?clause2 ...)
     (let ((temp ?test))
       (if temp (?result temp) (cond ?clause ?clause2 ...))))
    
    ((cond (?test) ?clause ?clause2 ...)
     (or ?test (cond ?clause ?clause2 ...)))
    
    ((cond (?test ?result ?result2 ...)
           ?clause ?clause2 ...)
     (if ?test
         (begin ?result ?result2 ...)
         (cond ?clause ?clause2 ...)))))

; The R4RS says a <step> may be omitted.
; That's a good excuse for a macro-defining macro that uses LETREC-SYNTAX
; and the ... escape.

(define-syntax do
  (syntax-rules ()
    ((do (?bindings0 ...) (?test) ?body0 ...)
     (do (?bindings0 ...) (?test (if #f #f)) ?body0 ...))
    ((do (?bindings0 ...) ?clause0 ?body0 ...)
     (letrec-syntax
       ((do-aux
         (... (syntax-rules ()
                ((do-aux () ((?name ?init ?step) ...) ?clause ?body ...)
                 (letrec ((loop (lambda (?name ...)
                                  (cond ?clause
                                        (else
                                         (begin #t ?body ...)
                                         (loop ?step ...))))))
                   (loop ?init ...)))
                ((do-aux ((?name ?init ?step) ?todo ...)
                         (?bindings ...)
                         ?clause
                         ?body ...)
                 (do-aux (?todo ...)
                         (?bindings ... (?name ?init ?step))
                         ?clause
                         ?body ...))
                ((do-aux ((?name ?init) ?todo ...)
                         (?bindings ...)
                         ?clause
                         ?body ...)
                 (do-aux (?todo ...)
                         (?bindings ... (?name ?init ?name))
                         ?clause
                         ?body ...))))))
       (do-aux (?bindings0 ...) () ?clause0 ?body0 ...)))))

(define-syntax delay
  (syntax-rules ()
    ((delay ?e) (.make-promise (lambda () ?e)))))

; Another use of LETREC-SYNTAX and the escape extension.

(define-syntax case
  (syntax-rules (else)
    ((case ?e1 (else ?body ?body2 ...))
     (begin ?e1 ?body ?body2 ...))
    ((case ?e1 (?z ?body ?body2 ...))
     (if (memv ?e1 '?z) (begin ?body ?body2 ...)))
    ((case ?e1 ?clause1 ?clause2 ?clause3 ...)
     (letrec-syntax
       ((case-aux
          (... (syntax-rules (else)
                ((case-aux ?temp (else ?body ?body2 ...))
                 (begin ?body ?body2 ...))
                ((case-aux ?temp ((?z ...) ?body ?body2 ...))
                 (if (memv ?temp '(?z ...)) (begin ?body ?body2 ...)))
                ((case-aux ?temp ((?z ...) ?body ?body2 ...) ?c1 ?c2 ...)
                 (if (memv ?temp '(?z ...))
                     (begin ?body ?body2 ...)
                     (case-aux ?temp ?c1 ?c2 ...)))
                ; a popular extension
                ((case-aux ?temp (?z ?body ?body2 ...) ?c1 ...)
                 (case-aux ?temp ((?z) ?body ?body2 ...) ?c1 ...))))))
       (let ((temp ?e1))
         (case-aux temp ?clause1 ?clause2 ?clause3 ...))))))

; A complete implementation of quasiquote, obtained by translating
; Jonathan Rees's implementation that was posted to RRRS-AUTHORS
; on 22 December 1986.
; Unfortunately, the use of LETREC scope means that it is vulnerable
; to top-level redefinitions of QUOTE etc.  That could be fixed, but
; it has hair enough already.

(begin
 
 (define-syntax .finalize-quasiquote letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.finalize-quasiquote quote ?arg ?return)
     (.interpret-continuation ?return (quote ?arg)))
    ((.finalize-quasiquote unquote ?arg ?return)
     (.interpret-continuation ?return ?arg))
    ((.finalize-quasiquote unquote-splicing ?arg ?return)
     (syntax-error ",@ in illegal context" ?arg))
    ((.finalize-quasiquote ?mode ?arg ?return)
     (.interpret-continuation ?return (?mode . ?arg)))))
 
 ; The first two "arguments" to .descend-quasiquote and to
 ; .descend-quasiquote-pair are always identical.
 
 (define-syntax .descend-quasiquote letrec
   (syntax-rules (quasiquote unquote unquote-splicing)
    ((.descend-quasiquote `?y ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x (?level) ?return))
    ((.descend-quasiquote ,?y ?x () ?return)
     (.interpret-continuation ?return unquote ?y))
    ((.descend-quasiquote ,?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote ,@?y ?x () ?return)
     (.interpret-continuation ?return unquote-splicing ?y))
    ((.descend-quasiquote ,@?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote (?y . ?z) ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote #(?y ...) ?x ?level ?return)
     (.descend-quasiquote-vector ?x ?x ?level ?return))
    ((.descend-quasiquote ?y ?x ?level ?return)
     (.interpret-continuation ?return quote ?x))))
 
 (define-syntax .descend-quasiquote-pair letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.descend-quasiquote-pair (?carx . ?cdrx) ?x ?level ?return)
     (.descend-quasiquote ?carx ?carx ?level (1 ?cdrx ?x ?level ?return)))))
 
 (define-syntax .descend-quasiquote-vector letrec
   (syntax-rules (quote)
    ((.descend-quasiquote-vector #(?y ...) ?x ?level ?return)
     (.descend-quasiquote (?y ...) (?y ...) ?level (6 ?x ?return)))))
 
 ; Representations for continuations used here.
 ; Continuation types 0, 1, 2, and 6 take a mode and an expression.
 ; Continuation types -1, 3, 4, 5, and 7 take just an expression.
 ;
 ; (-1)
 ;     means no continuation
 ; (0)
 ;     means to call .finalize-quasiquote with no further continuation
 ; (1 ?cdrx ?x ?level ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-pair
 ; (2 ?car-mode ?car-arg ?x ?return)
 ;     means a return from the second call to .descend-quasiquote in
 ;     in Jonathan's code for .descend-quasiquote-pair
 ; (3 ?car-arg ?return)
 ;     means take the result and return an append of ?car-arg with it
 ; (4 ?cdr-mode ?cdr-arg ?return)
 ;     means take the result and call .finalize-quasiquote on ?cdr-mode
 ;     and ?cdr-arg with a continuation of type 5
 ; (5 ?car-result ?return)
 ;     means take the result and return a cons of ?car-result onto it
 ; (6 ?x ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-vector
 ; (7 ?return)
 ;     means take the result and return a call of list->vector on it
 
 (define-syntax .interpret-continuation letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.interpret-continuation (-1) ?e) ?e)
    ((.interpret-continuation (0) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (-1)))    
    ((.interpret-continuation (1 ?cdrx ?x ?level ?return) ?car-mode ?car-arg)
     (.descend-quasiquote ?cdrx
                          ?cdrx
                          ?level
                          (2 ?car-mode ?car-arg ?x ?return)))    
    ((.interpret-continuation (2 quote ?car-arg ?x ?return) quote ?cdr-arg)
     (.interpret-continuation ?return quote ?x))    
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return) quote ())
     (.interpret-continuation ?return unquote ?car-arg))
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (3 ?car-arg ?return)))  
    ((.interpret-continuation (2 ?car-mode ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?car-mode ?car-arg (4 ?cdr-mode ?cdr-arg ?return)))
      
    ((.interpret-continuation (3 ?car-arg ?return) ?e)
     (.interpret-continuation ?return append (?car-arg ?e)))
    ((.interpret-continuation (4 ?cdr-mode ?cdr-arg ?return) ?e1)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (5 ?e1 ?return)))
    ((.interpret-continuation (5 ?e1 ?return) ?e2)
     (.interpret-continuation ?return .cons (?e1 ?e2)))
    ((.interpret-continuation (6 ?x ?return) quote ?arg)
     (.interpret-continuation ?return quote ?x))
    ((.interpret-continuation (6 ?x ?return) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (7 ?return)))
    ((.interpret-continuation (7 ?return) ?e)
     (.interpret-continuation ?return .list->vector (?e)))))
 
 (define-syntax quasiquote letrec
   (syntax-rules ()
    ((quasiquote ?x)
     (.descend-quasiquote ?x ?x () (0)))))
 )

(define-syntax let*-syntax
  (syntax-rules ()
    ((let*-syntax () ?body)
     (let-syntax () ?body))
    ((let*-syntax ((?name1 ?val1) (?name ?val) ...) ?body)
     (let-syntax ((?name1 ?val1)) (let*-syntax ((?name ?val) ...) ?body)))))


; Some new syntax

; (parameterize ((p1 e1) ...) b1 b2 ...)
; where each p1 is the name of a parameter (a procedure of 0 or 1 args).

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((p1 e1) ...) b1 b2 ...)
     (letrec-syntax 
         ((parameterize-aux
           (... (syntax-rules ()
                  ((parameterize-aux (t ...) ((p0 e0) x ...) body1 body2 ...)
                   (let ((temp e0))
                     (parameterize-aux ((temp e0 p0) t ...) 
                                       (x ...) 
                                       body1 body2 ...)))
                  ((parameterize-aux ((t e p) ...) () body1 body2 ...)
                   (let-syntax ((swap!
                                 (syntax-rules ()
                                   ((swap! var param)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (param tmp))))))
                     (dynamic-wind
                      (lambda ()
                        (swap! t p) ...)
                      (lambda ()
                        body1 body2 ...)
                      (lambda ()
                        (swap! t p) ...))))))))
       (parameterize-aux () ((p1 e1) ...) b1 b2 ...)))))

; SRFI-11 LET-VALUES and LET*-VALUES

(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
   
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
   
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
   
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
   
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
   
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body)))))) 

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))
    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
       (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

; For the macro-expander; will be redefined later.

(define-syntax .call
  (syntax-rules ()
    ((.call ?inlines ?op ?exp)
     ?exp)))

))  ; end of (for-each (lambda (x) (macro-expand ...)) ...)

(define-syntax-scope 'letrec)

