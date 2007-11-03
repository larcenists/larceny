;;;=====================================================================
;;;
;;; Derived forms:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;; Modified for Larceny.
;;;
;;; $Id$
;;;
;;;=====================================================================  

;;;=====================================================================
;;;
;;; This file builds r6rs up using a sequence of libraries.
;;; It constitutes a nontrivial example, tutorial and test
;;; of the library system.  
;;;
;;; It is meant to be expanded by expander.scm and compiled 
;;; together with the latter before using in a production system.
;;;
;;; Various of the standard macros were copied from
;;; SRFI-93 reference implementation.
;;;
;;; An explicit renaming library is included for easier 
;;; porting of legacy macros in some implementations.
;;;
;;;=====================================================================

(library (core primitives)
  
  (export
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ... syntax syntax-case
      
   ;; Procedures and values defined in core expander:
   
   (rename (ex:make-variable-transformer make-variable-transformer)
           (ex:identifier?               identifier?)
           (ex:bound-identifier=?        bound-identifier=?)
           (ex:free-identifier=?         free-identifier=?)
           (ex:generate-temporaries      generate-temporaries) 
           (ex:datum->syntax             datum->syntax)
           (ex:syntax->datum             syntax->datum)
           (ex:syntax-violation          syntax-violation)
           (ex:environment               environment)
           (ex:environment-bindings      environment-bindings)
           (ex:eval                      eval)
           (ex:undefined                 undefined)))
  
  (import
   
   (only (core primitive-macros)
     
     begin if set! and or lambda quote
     define define-syntax let-syntax letrec-syntax 
     syntax syntax-case _ ...)
   
   ;; An extension to the r6rs import syntax, used here to make  
   ;; available variable bindings provided natively.
   ;; This will not work for macros, which have to be defined
   ;; within the context of this expander.  
   
   (primitives
   
    ;; Procedures and values defined in the core expander:
    
    ex:make-variable-transformer ex:identifier? ex:bound-identifier=?
    ex:free-identifier=? ex:generate-temporaries ex:datum->syntax ex:syntax->datum 
    ex:syntax-violation ex:environment ex:environment-bindings ex:eval
    ex:undefined
    ))
  
  ) ;; core primitives

(library (core with-syntax)
  (export with-syntax)
  (import (for (core primitives) run expand)
          (primitives list)) 
  
  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)             (syntax (begin e1 e2 ...)))
        ((_ ((out in)) e1 e2 ...)     (syntax (syntax-case in ()
                                                (out (begin e1 e2 ...)))))
        ((_ ((out in) ...) e1 e2 ...) (syntax (syntax-case (list in ...) ()
                                                ((out ...) (begin e1 e2 ...))))))))
  )

(library (core syntax-rules)
  (export syntax-rules)
  (import (for (core primitives)        expand run)
          (for (core with-syntax)       expand)
          (for (primitives for-all map) expand))
  
  (define-syntax syntax-rules
    (lambda (x)
      (define clause
        (lambda (y)
          (syntax-case y ()
            (((keyword . pattern) template)
             (syntax ((dummy . pattern) (syntax template))))
            (_
             (syntax-violation 'syntax-rules "Invalid expression" x)))))
      (syntax-case x ()
        ((_ (k ...) cl ...)
         (for-all identifier? (syntax (k ...)))
         (with-syntax (((cl ...) (map clause (syntax (cl ...)))))
           (syntax
            (lambda (x) (syntax-case x (k ...) cl ...))))))))
  )

(library (core let)
  (export let letrec letrec*)
  (import (for (core primitives)        expand run)
          (for (core with-syntax)       expand)
          (for (primitives for-all)     expand))
  
  (define-syntax let
    (lambda (x)
      (syntax-case x ()
        ((_ ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (x ...)))
         (syntax ((lambda (x ...) e1 e2 ...) v ...)))
        ((_ f ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (f x ...)))
         (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f) v ...))))))
  
  (define-syntax letrec
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (with-syntax (((t ...) (generate-temporaries (syntax (i ...)))))
           (syntax (let ((i undefined) ...)
                     (let ((t v) ...)
                       (set! i t) ...
                       (let () e1 e2 ...)))))))))
  
  (define-syntax letrec*
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (syntax (let ()
                   (define i v) ...
                   (let () e1 e2 ...)))))))
  
  ) ; let

(library (core derived)
  (export let* cond case else =>)   
  (import (for (core primitives)       expand run)
          (for (core let)              expand run)
          (for (core with-syntax)      expand)
          (for (core syntax-rules)     expand)
          (for (primitives for-all null? memv car cdr) expand run))
  
  (define-syntax let*
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)
         (syntax (let () e1 e2 ...)))
        ((_ ((x v) ...) e1 e2 ...)
         (for-all identifier? (syntax (x ...)))
         (let f ((bindings (syntax ((x v) ...))))
           (syntax-case bindings ()
             (((x v))        (syntax (let ((x v)) e1 e2 ...)))
             (((x v) . rest) (with-syntax ((body (f (syntax rest))))
                               (syntax (let ((x v)) body))))))))))
  
  (define-syntax cond
    (lambda (x)
      (syntax-case x ()
        ((_ c1 c2 ...)
         (let f ((c1  (syntax c1))
                 (c2* (syntax (c2 ...))))
           (syntax-case c2* ()
             (()
              (syntax-case c1 (else =>)
                ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                ((e0)             (syntax (let ((t e0)) (if t t))))
                ((e0 => e1)       (syntax (let ((t e0)) (if t (e1 t)))))
                ((e0 e1 e2 ...)   (syntax (if e0 (begin e1 e2 ...))))
                (_                (syntax-violation 'cond "Invalid expression" x))))
             ((c2 c3 ...)
              (with-syntax ((rest (f (syntax c2)
                                     (syntax (c3 ...)))))
                (syntax-case c1 (else =>)
                  ((e0)           (syntax (let ((t e0)) (if t t rest))))
                  ((e0 => e1)     (syntax (let ((t e0)) (if t (e1 t) rest))))
                  ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                  (_              (syntax-violation 'cond "Invalid expression" x)))))))))))
  
  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e c1 c2 ...)
         (with-syntax ((body
                        (let f ((c1 (syntax c1))
                                (cmore (syntax (c2 ...))))
                          (if (null? cmore)
                              (syntax-case c1 (else)
                                ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                                (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                                 (begin e1 e2 ...)))))
                              (with-syntax ((rest (f (car cmore) (cdr cmore))))
                                (syntax-case c1 ()
                                  (((k ...) e1 e2 ...)
                                   (syntax (if (memv t '(k ...))
                                               (begin e1 e2 ...)
                                               rest)))))))))
           (syntax (let ((t e)) body)))))))
  
  (define-syntax =>
    (lambda (x)
      (syntax-violation '=> "Invalid expression" x)))
  
  (define-syntax else
    (lambda (x)
      (syntax-violation 'else "Invalid expression" x)))
  
  ) ; derived

(library (core identifier-syntax)
  (export identifier-syntax)
  (import (for (core primitives) 
            expand 
            run
            ;; since generated macro contains (syntax set!) at level 0
            (meta -1))) 
  
  (define-syntax identifier-syntax
    (lambda (x)
      (syntax-case x (set!)
        ((_ e)
         (syntax (lambda (x)
                   (syntax-case x ()
                     (id (identifier? (syntax id)) (syntax e))
                     ((_ x (... ...))              (syntax (e x (... ...))))))))
        ((_ (id exp1) 
            ((set! var val) exp2))
         (and (identifier? (syntax id)) 
              (identifier? (syntax var)))
         (syntax 
          (make-variable-transformer
           (lambda (x)
             (syntax-case x (set!)
               ((set! var val)               (syntax exp2))
               ((id x (... ...))             (syntax (exp1 x (... ...))))
               (id (identifier? (syntax id)) (syntax exp1))))))))))
  )

;;;=========================================================
;;;
;;; Quasisyntax in terms of syntax-case.
;;;
;;;=========================================================
;;;
;;; To make nested unquote-splicing behave in a useful way,
;;; the R5RS-compatible extension of quasiquote in appendix B
;;; of the following paper is here ported to quasisyntax:
;;;
;;; Alan Bawden - Quasiquotation in Lisp
;;; http://citeseer.ist.psu.edu/bawden99quasiquotation.html
;;;
;;; The algorithm converts a quasisyntax expression to an
;;; equivalent with-syntax expression.
;;; For example:
;;;
;;; (quasisyntax (set! #,a #,b))
;;;   ==> (with-syntax ((t0 a)
;;;                     (t1 b))
;;;         (syntax (set! t0 t1)))
;;;
;;; (quasisyntax (list #,@args))
;;;   ==> (with-syntax (((t ...) args))
;;;         (syntax (list t ...)))
;;;
;;; Note that quasisyntax is expanded first, before any
;;; ellipses act.  For example:
;;;
;;; (quasisyntax (f ((b #,a) ...))
;;;   ==> (with-syntax ((t a))
;;;         (syntax (f ((b t) ...))))
;;;
;;; so that
;;;
;;; (let-syntax ((test-ellipses-over-unsyntax
;;;               (lambda (e)
;;;                 (let ((a (syntax a)))
;;;                   (with-syntax (((b ...) (syntax (1 2 3))))
;;;                     (quasisyntax
;;;                      (quote ((b #,a) ...))))))))
;;;   (test-ellipses-over-unsyntax))
;;;
;;;     ==> ((1 a) (2 a) (3 a))

(library (core quasisyntax)
  (export quasisyntax unsyntax unsyntax-splicing) 
  (import (for (core primitives)  run expand)
          (for (core let)         run expand) 
          (for (core derived)     run expand)
          (for (core with-syntax) run expand)
          (for (primitives = > + - vector->list) run expand)) 
  
  (define-syntax quasisyntax
    (lambda (e)
      
      ;; Expand returns a list of the form
      ;;    [template[t/e, ...] (replacement ...)]
      ;; Here template[t/e ...] denotes the original template
      ;; with unquoted expressions e replaced by fresh
      ;; variables t, followed by the appropriate ellipses
      ;; if e is also spliced.
      ;; The second part of the return value is the list of
      ;; replacements, each of the form (t e) if e is just
      ;; unquoted, or ((t ...) e) if e is also spliced.
      ;; This will be the list of bindings of the resulting
      ;; with-syntax expression.
      
      (define (expand x level)
        (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
          ((quasisyntax e)
           (with-syntax (((k _)     x) ;; original identifier must be copied
                         ((e* reps) (expand (syntax e) (+ level 1))))
             (syntax ((k e*) reps))))                                  
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax (t ((t e))))))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (syntax ((t ... . r*)
                      ((t e) ... rep ...)))))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
               (syntax ((t ... ... . r*)
                        (((t ...) e) ... rep ...))))))
          ((k . r)
           (and (> level 0)
                (identifier? (syntax k))
                (or (free-identifier=? (syntax k) (syntax unsyntax))
                    (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
           (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
             (syntax ((k . r*) reps))))
          ((h . t)
           (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                         ((t* (rep2 ...)) (expand (syntax t) level)))
             (syntax ((h* . t*)
                      (rep1 ... rep2 ...)))))
          (#(e ...)                                                               
           (with-syntax ((((e* ...) reps)
                          (expand (vector->list (syntax #(e ...))) level)))
             (syntax (#(e* ...) reps))))
          (other
           (syntax (other ())))))
      
      (syntax-case e ()
        ((_ template)
         (with-syntax (((template* replacements) (expand (syntax template) 0)))
           (syntax
            (with-syntax replacements (syntax template*))))))))
  
  (define-syntax unsyntax
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))
  
  (define-syntax unsyntax-splicing
    (lambda (e)
      (syntax-violation 'unsyntax "Invalid expression" e)))
  )

(library (core quasiquote)
  (export quasiquote unquote unquote-splicing)
  (import (for (core primitives)  run expand)
          (for (core let)         run expand) 
          (for (core derived)     run expand)
          (for (core with-syntax) expand)
          (for (core quasisyntax) expand)
          (for (primitives = + - null? cons car cdr append map list vector list->vector) 
            run expand)) 
  
  ;; Optimised version copied from portable syntax-case (Dybvig)
  
  (define-syntax quasiquote
    (let ()
      (define (quasi p lev)
        (syntax-case p (unquote quasiquote)
          ((unquote p)
           (if (= lev 0)
               (syntax ("value" p))
               (quasicons (syntax ("quote" unquote)) (quasi (syntax (p)) (- lev 1)))))
          ((quasiquote p) (quasicons (syntax ("quote" quasiquote)) (quasi (syntax (p)) (+ lev 1))))
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (quasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote-splicing)) (quasi (syntax (p ...)) (- lev 1)))
                   (quasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))))
          (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
          (p (syntax ("quote" p)))))
      (define (vquasi p lev)
        (syntax-case p ()
          ((p . q)
           (syntax-case (syntax p) (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons (syntax ("quote" unquote)) (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend (syntax (("value" p) ...)) (vquasi (syntax q) lev))
                  (quasicons
                   (quasicons
                    (syntax ("quote" unquote-splicing))
                    (quasi (syntax (p ...)) (- lev 1)))
                   (vquasi (syntax q) lev))))
             (_ (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
          (() (syntax ("quote" ())))))
      (define (quasicons x y)
        (with-syntax ((x x) (y y))
          (syntax-case (syntax y) ()
            (("quote" dy)
             (syntax-case (syntax x) ()
               (("quote" dx) (syntax ("quote" (dx . dy))))
               (_ (if (null? (syntax dy)) (syntax ("list" x)) (syntax ("list*" x y))))))
            (("list" . stuff) (syntax ("list" x . stuff)))
            (("list*" . stuff) (syntax ("list*" x . stuff)))
            (_ (syntax ("list*" x y))))))
      (define (quasiappend x y)
        (syntax-case y ()
          (("quote" ())
           (cond
             ((null? x) (syntax ("quote" ())))
             ((null? (cdr x)) (car x))
             (else (with-syntax (((p ...) x)) (syntax ("append" p ...))))))
          (_
           (cond
             ((null? x) y)
             (else (with-syntax (((p ...) x) (y y)) (syntax ("append" p ... y))))))))
      (define (quasilist* x y)
        (let f ((x x))
          (if (null? x)
              y
              (quasicons (car x) (f (cdr x))))))
      (define (quasivector x)
        (syntax-case x ()
          (("quote" (x ...)) (syntax ("quote" #(x ...))))
          (_
           (let f ((y x) (k (lambda (ls) (quasisyntax ("vector" (unsyntax-splicing ls))))))
             (syntax-case y ()
               (("quote" (y ...)) (k (syntax (("quote" y) ...))))
               (("list" y ...) (k (syntax (y ...))))
               (("list*" y ... z) (f (syntax z) (lambda (ls) (k (append (syntax (y ...)) ls)))))
               (else (quasisyntax ("list->vector" (unsyntax x)))))))))
      (define (emit x)
        (syntax-case x ()
          (("quote" x) (syntax 'x))
          (("list" x ...) (quasisyntax (list (unsyntax-splicing (map emit (syntax (x ...)))))))
          ;; could emit list* for 3+ arguments if implementation supports list*
          (("list*" x ... y)
           (let f ((x* (syntax (x ...))))
             (if (null? x*)
                 (emit (syntax y))
                 (quasisyntax (cons (unsyntax (emit (car x*))) (unsyntax (f (cdr x*))))))))
          (("append" x ...) (quasisyntax (append (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("vector" x ...) (quasisyntax (vector (unsyntax-splicing (map emit (syntax (x ...)))))))
          (("list->vector" x) (quasisyntax (list->vector (unsyntax (emit (syntax x))))))
          (("value" x) (syntax x))))
      (lambda (x)
        (syntax-case x ()
          ;; convert to intermediate language, combining introduced (but not
          ;; unquoted source) quote expressions where possible and choosing
          ;; optimal construction code otherwise, then emit Scheme code
          ;; corresponding to the intermediate language forms.
          ((_ e) (emit (quasi (syntax e) 0)))))))
  
  (define-syntax unquote
    (lambda (e)
      (syntax-violation 'unquote "Invalid expression" e)))
  
  (define-syntax unquote-splicing
    (lambda (e)
      (syntax-violation 'unquote-splicing "Invalid expression" e)))
  )

(library (core let-values)
  (export let-values let*-values)
  (import (for (core primitives)   expand run)
          (for (core syntax-rules) expand)
          (core let)
          (primitives call-with-values))
  
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
  
  ) ; core let-values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Library added for Larceny.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (larceny deprecated)             ; [Larceny]
  (export issue-warning-deprecated)
  (import
   (primitives
    issue-warning-deprecated)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS standard libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records procedural)
  (export
   make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)
  (import
   (primitives
    make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS optional libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records inspection)
  (export
   record? record-rtd rtd-name rtd-parent
   rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import
   (primitives
    record? record-rtd rtd-name rtd-parent
    rtd-field-names rtd-all-field-names rtd-field-mutable?)))

; FIXME: (err5rs records syntactic) not yet implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS standard libraries.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs base (6))         
  
  (export 
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   assert
   quasiquote unquote unquote-splicing
   syntax-rules 
   identifier-syntax
   
   ;; R5RS primitives:
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char? char->integer char=? char<? char>? char<=? char>=?
   complex? cons cos 
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?
   
   ;; R6RS additional procedures:
   
   real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
   nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
   symbol=? string-for-each vector-map vector-for-each error assertion-violation
   call/cc)
  
  (import (except (core primitives) _ ...)     
          (core let)                          
          (core derived)             
          (core quasiquote)        
          (core let-values)
          (for (core syntax-rules)      expand)   
          (for (core identifier-syntax) expand)
          (for (only (core primitives) _ ... set!) expand)
          (primitives 
           
           ;; R5RS primitives:
           
           * + - / < <= = > >= abs acos append apply asin atan 
           boolean? call-with-current-continuation 
           call-with-values car cdr caar cadr cdar cddr
           caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
           cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
           ceiling char? char->integer char=? char<? char>? char<=? char>=?
           complex? cons cos 
           denominator dynamic-wind 
           eq? equal? eqv? even? exact? exp expt floor for-each
           gcd imag-part inexact? integer->char integer?
           lcm length list list->string
           list->vector list-ref list-tail list? log magnitude make-polar
           make-rectangular make-string make-vector map max min
           negative? not null? number->string number? numerator
           odd? pair? 
           positive? procedure? rational? rationalize
           real-part real? reverse round
           sin sqrt string string->list string->number string->symbol
           string-append 
           string-copy string-length string-ref string<=? string<?
           string=? string>=? string>? string? substring symbol->string symbol? tan
           truncate values vector vector->list
           vector-fill! vector-length vector-ref vector-set! vector? zero?
           
           ;; R6RS additional procedures:
           
           real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
           nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
           symbol=? string-for-each vector-map vector-for-each error assertion-violation
           call/cc))
  
    (define-syntax assert
      (syntax-rules ()
        ((_ expression)
         (if (not expression)
             (assertion-violation #f "assertion failed" 'expression)))))
  
  ) ;; rnrs base

(library (rnrs unicode (6))
  
  (export
   char-upcase char-downcase char-titlecase char-foldcase
   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
   char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? char-title-case?
   char-general-category
   string-upcase string-downcase string-titlecase string-foldcase
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   string-normalize-nfd string-normalize-nfkd
   string-normalize-nfc string-normalize-nfkc)

  (import
   (primitives
    char-upcase char-downcase char-titlecase char-foldcase
    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
    char-alphabetic? char-numeric? char-whitespace?
    char-upper-case? char-lower-case? char-title-case?
    char-general-category
    string-upcase string-downcase string-titlecase string-foldcase
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    string-normalize-nfd string-normalize-nfkd
    string-normalize-nfc string-normalize-nfkc))
  )

(library (rnrs bytevectors (6))
  (export
   endianness                                  ; deprecated
   native-endianness

   bytevector? make-bytevector bytevector-length
   bytevector=?
   bytevector-fill! bytevector-copy! bytevector-copy

   bytevector-u8-ref bytevector-s8-ref
   bytevector-u8-set! bytevector-s8-set!
   bytevetor->u8-list u8-list->bytevector

   bytevector-uint-ref bytevector-sint-ref
   bytevector-uint-set! bytevector-sint-set!
   bytevetor->uint-list bytevetor->sint-list
   uint-list->bytevector sint-list->bytevector

   bytevector-u16-ref bytevector-s16-ref
   bytevector-u16-native-ref bytevector-s16-native-ref
   bytevector-u16-set! bytevector-s16-set!
   bytevector-u16-native-set! bytevector-s16-native-set!

   bytevector-u32-ref bytevector-s32-ref
   bytevector-u32-native-ref bytevector-s32-native-ref
   bytevector-u32-set! bytevector-s32-set!
   bytevector-u32-native-set! bytevector-s32-native-set!

   bytevector-u64-ref bytevector-s64-ref
   bytevector-u64-native-ref bytevector-s64-native-ref
   bytevector-u64-set! bytevector-s64-set!
   bytevector-u64-native-set! bytevector-s64-native-set!

   bytevector-ieee-single-native-ref
   bytevector-ieee-single-ref
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-set!
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-set!

   string->utf8 string->utf16 string->utf32
   utf8->string utf16->string utf32->string)

  (import
   (core primitives)
   (for (core syntax-rules) expand)
   (larceny deprecated)
   (primitives
    native-endianness

    bytevector? make-bytevector bytevector-length
    bytevector=?
    bytevector-fill! bytevector-copy! bytevector-copy

    bytevector-u8-ref bytevector-s8-ref
    bytevector-u8-set! bytevector-s8-set!
    bytevetor->u8-list u8-list->bytevector

    bytevector-uint-ref bytevector-sint-ref
    bytevector-uint-set! bytevector-sint-set!
    bytevetor->uint-list bytevetor->sint-list
    uint-list->bytevector sint-list->bytevector

    bytevector-u16-ref bytevector-s16-ref
    bytevector-u16-native-ref bytevector-s16-native-ref
    bytevector-u16-set! bytevector-s16-set!
    bytevector-u16-native-set! bytevector-s16-native-set!

    bytevector-u32-ref bytevector-s32-ref
    bytevector-u32-native-ref bytevector-s32-native-ref
    bytevector-u32-set! bytevector-s32-set!
    bytevector-u32-native-set! bytevector-s32-native-set!

    bytevector-u64-ref bytevector-s64-ref
    bytevector-u64-native-ref bytevector-s64-native-ref
    bytevector-u64-set! bytevector-s64-set!
    bytevector-u64-native-set! bytevector-s64-native-set!

    bytevector-ieee-single-native-ref
    bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref
    bytevector-ieee-double-ref
    bytevector-ieee-single-native-set!
    bytevector-ieee-single-set!
    bytevector-ieee-double-native-set!
    bytevector-ieee-double-set!

    string->utf8 string->utf16 string->utf32
    utf8->string utf16->string utf32->string))

  ; [Larceny]
  ; In Larceny, *every* symbol describes an endianness.
  ; See Lib/Common/bytevector.sch for semantics.

  (define-syntax endianness
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'endianness)
       (quote x)))))

  ) ; rnrs bytevectors

(library (rnrs lists (6))
  (export find for-all exists filter partition fold-left fold-right
          remp remove remq remv memp member memv memq
          assp assoc assv assq)
  (import (primitives 
           find for-all exists filter partition fold-left fold-right
           remp remove remq remv memp member memv memq
           assp assoc assv assq)))  

(library (rnrs sorting (6))
  (export list-sort vector-sort vector-sort!)
  (import (primitives list-sort vector-sort vector-sort!)))

(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (for (core primitives)   expand run)
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand)
          (for (primitives not map length assertion-violation = >= apply)
            expand run) )
  
  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))
  
  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))
  
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation 'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (begin c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (begin e1 e2 ...)
                                        (begin c ... (do step ...))))))))))))                         
  
  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))
  
  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "unexpected number of arguments"))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (length '(x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (length '(x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                  args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))                                      
  
  ) ; rnrs control                                      

(library (rnrs records procedural (6))
  (export
   make-record-type-descriptor record-type-descriptor?
   make-record-constructor-descriptor record-constructor
   record-predicate record-accessor record-mutator)
  (import
   (primitives
    make-record-type-descriptor record-type-descriptor?
    make-record-constructor-descriptor record-constructor
    record-predicate record-accessor record-mutator)))

(library (rnrs records inspection (6))
  (export
   record? record-rtd record-type-name record-type-parent record-type-uid
   record-type-generative? record-type-sealed? record-type-opaque?
   record-type-field-names record-field-mutable?)
  (import
   (primitives
    record? record-rtd record-type-name record-type-parent record-type-uid
    record-type-generative? record-type-sealed? record-type-opaque?
    record-type-field-names record-field-mutable?)))

; FIXME: (rnrs records syntactic (6)) not yet implemented.

; FIXME: (rnrs exceptions (6)) not yet implemented.

(library (rnrs conditions (6))

  (export
   &condition condition simple-conditions condition?
   condition-predicate condition-accessor
   define-condition-type
   &message make-message-condition message-condition? condition-message
   &warning make-warning warning?
   &serious make-serious-condition serious-condition?
   &error make-error error?
   &violation make-violation violation?
   &assertion make-assertion-violation assertion-violation?
   &irritants make-irritants-condition irritants-condition? condition-irritants
   &who make-who-condition who-condition? condition-who
   &non-continuable make-non-continuable-violation non-continuable-violation?
   &implementation-restriction make-implementation-restriction-violation
   implementation-restriction-violation?
   &lexical make-lexical-violation lexical-violation?
   &syntax make-syntax-violation syntax-violation?
   syntax-violation-form syntax-violation-subform
   &undefined make-undefined-violation undefined-violation?)

  (import
   (rnrs base)
   (primitives
    &condition condition simple-conditions condition?
    condition-predicate condition-accessor
    &message make-message-condition message-condition? condition-message
    &warning make-warning warning?
    &serious make-serious-condition serious-condition?
    &error make-error error?
    &violation make-violation violation?
    &assertion make-assertion-violation assertion-violation?
    &irritants make-irritants-condition irritants-condition?
    condition-irritants
    &who make-who-condition who-condition? condition-who
    &non-continuable make-non-continuable-violation non-continuable-violation?
    &implementation-restriction make-implementation-restriction-violation
    implementation-restriction-violation?
    &lexical make-lexical-violation lexical-violation?
    &syntax make-syntax-violation syntax-violation?
    syntax-violation-form syntax-violation-subform
    &undefined make-undefined-violation undefined-violation?))

  ; Note: This relies on letrec* semantics for internal definitions.

  (define-syntax define-condition-type
    (syntax-rules ()
     ((define-condition-type <condition-type>
        <supertype> <constructor> <predicate> (<field> <accessor>) ...)
      (begin
       (define <condition-type>
         (make-rtd '<condition-type>
                   (vector (list 'immutable '<field>) ...)
                   <supertype>))
       (define <constructor> (rtd-constructor <condition-type>))
       (define <predicate> (condition-predicate <condition-type>))
       (define <accessor>
         (condition-accessor <condition-type>
                             (rtd-accessor <condition-type> '<field>)))
       ...)))))

(library (rnrs io ports (6))

  (export

   &i/o make-i/o-error i/o-error?
   &i/o-read make-i/o-read-error i/o-read-error?
   &i/o-write make-i/o-write-error i/o-write-error?
   &i/o-invalid-position make-i/o-invalid-position-error
   i/o-invalid-position-error? i/o-error-position
   &i/o-filename make-i/o-filename-error i/o-filename-error?
   i/o-error-filename
   &i/o-file-protection make-i/o-file-protection-error
   i/o-file-protection-error?
   &i/o-file-is-read-only make-i/o-file-is-read-only-error
   i/o-file-is-read-only-error?
   &i/o-file-already-exists make-i/o-file-already-exists-error
   i/o-file-already-exists-error?
   &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
   i/o-file-does-not-exist-error?
   &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

   file-options                           ; deprecated syntax
   buffer-mode                            ; deprecated syntax
   buffer-mode?                           ; deprecated procedure

   latin-1-codec utf-8-codec utf-16-codec

   eol-style                              ; deprecated syntax
   native-eol-style

   &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
   &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
   i/o-encoding-error-char

   error-handling-mode                    ; deprecated syntax

   make-transcoder
   native-transcoder
   transcoder-codec transcoder-eol-style transcoder-error-handling-mode

   bytevector->string string->bytevector

   eof-object eof-object?

   port? port-transcoder textual-port? binary-port? transcoded-port
   port-has-port-position? port-position
   port-has-set-port-position!? set-port-position!
   close-port call-with-port

   input-port? port-eof?
   open-file-input-port open-bytevector-input-port open-string-input-port
   standard-input-port current-input-port
   make-custom-binary-input-port make-custom-textual-input-port

   get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
   get-bytevector-some                    ; deprecated procedure
   get-bytevector-all

   get-char lookahead-char
   get-string-n get-string-n! get-string-all get-line get-datum

   output-port? flush-output-port output-port-buffer-mode
   open-file-output-port
   open-bytevector-output-port            ; deprecated procedure
   open-string-output-port                ; deprecated procedure
   call-with-bytevector-output-port
   call-with-string-output-port
   standard-output-port current-output-port current-error-port
   make-custom-binary-output-port make-custom-textual-output-port

   put-u8 put-bytevector

   put-char put-string put-datum

   open-file-input/output-port
   make-custom-binary-input/output-port
   make-custom-textual-input/output-port
   )

  (import
   (core primitives)
   (for (only (core primitives) ...) expand)
   (for (core syntax-rules) expand)
   (rnrs base)
   (larceny deprecated) ; [Larceny]
   (primitives

    &i/o make-i/o-error i/o-error?
    &i/o-read make-i/o-read-error i/o-read-error?
    &i/o-write make-i/o-write-error i/o-write-error?
    &i/o-invalid-position make-i/o-invalid-position-error
    i/o-invalid-position-error? i/o-error-position
    &i/o-filename make-i/o-filename-error i/o-filename-error?
    i/o-error-filename
    &i/o-file-protection make-i/o-file-protection-error
    i/o-file-protection-error?
    &i/o-file-is-read-only make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    &i/o-file-already-exists make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

    buffer-mode?

    latin-1-codec utf-8-codec utf-16-codec

    native-eol-style

    &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
    &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
    i/o-encoding-error-char

    make-transcoder
    native-transcoder
    transcoder-codec transcoder-eol-style transcoder-error-handling-mode

    bytevector->string string->bytevector

    eof-object eof-object?

    port? port-transcoder textual-port? binary-port? transcoded-port
    port-has-port-position? port-position
    port-has-set-port-position!? set-port-position!
    close-port call-with-port

    input-port? port-eof?
    open-file-input-port open-bytevector-input-port open-string-input-port
    standard-input-port current-input-port
    make-custom-binary-input-port make-custom-textual-input-port

    get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
    get-bytevector-some                    ; deprecated procedure
    get-bytevector-all

    get-char lookahead-char
    get-string-n get-string-n! get-string-all get-line get-datum

    output-port? flush-output-port output-port-buffer-mode
    open-file-output-port
    open-bytevector-output-port            ; deprecated procedure
    open-string-output-port                ; deprecated procedure
    call-with-bytevector-output-port
    call-with-string-output-port
    standard-output-port current-output-port current-error-port
    make-custom-binary-output-port make-custom-textual-output-port

    put-u8 put-bytevector

    put-char put-string put-datum

    open-file-input/output-port
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    ))

  ; [Larceny]
  ; Larceny accepts any symbol as a file option,
  ; but ignores all but a few options.

  (define-syntax file-options
    (syntax-rules ()
     ((_ opt ...)
      (make-file-options 'opt ...))))

  ; [Larceny]
  ; FIXME:  This should return an enumeration set,
  ; although how Larceny is supposed to do that given
  ; its infinite set of legal file options is a mystery.

  (define make-file-options list)

  ; [Larceny]
  ; In Larceny, *every* symbol describes a buffer mode.
  ; See Lib/Common/portio.sch for semantics.
  ;
  ; The three buffer modes allowed by the current draft R6RS
  ; do not include Larceny's traditional discretionary-flush
  ; mode for interactive ports.  Beginning in Larceny v0.94,
  ; the preferred name of this buffer mode is datum.
  ;
  ; As the current draft R6RS is written, however, the
  ; buffer-mode syntax accepts only three symbols:
  ;
  ;     none, line, block
  ;
  ; Programmers should therefore get into the habit of
  ; specifying buffer modes using Scheme's traditional
  ; quote syntax.
  ;
  ; FIXME:  Syntax checking should be done at macro expansion time,
  ; although this particular syntax check is so stupid it really
  ; shouldn't be done at all.

  (define-syntax buffer-mode
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'buffer-mode)
       (let ((mode (quote x)))
         (if (memq mode '(none line block))
             mode
             (assertion-violation 'buffer-mode
                                  "Larceny-specific buffer mode"
                                  mode)))))))

  ; [Larceny]
  ; In Larceny, *every* symbol describes an eol-style.
  ; See Lib/Common/portio.sch for semantics.

  (define-syntax eol-style
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'eol-style)
       (quote x)))))

  ; [Larceny]
  ; In Larceny, *every* symbol describes an error handling mode.
  ; See Lib/Common/portio.sch for semantics.

  (define-syntax error-handling-mode
    (syntax-rules ()
     ((_ x)
      (begin
       (issue-warning-deprecated 'error-handling-mode)
       (quote x)))))

  )

(library (rnrs io simple (6))
  (export

   &i/o make-i/o-error i/o-error?
   &i/o-read make-i/o-read-error i/o-read-error?
   &i/o-write make-i/o-write-error i/o-write-error?
   &i/o-invalid-position make-i/o-invalid-position-error
   i/o-invalid-position-error? i/o-error-position
   &i/o-filename make-i/o-filename-error i/o-filename-error?
   i/o-error-filename
   &i/o-file-protection make-i/o-file-protection-error
   i/o-file-protection-error?
   &i/o-file-is-read-only make-i/o-file-is-read-only-error
   i/o-file-is-read-only-error?
   &i/o-file-already-exists make-i/o-file-already-exists-error
   i/o-file-already-exists-error?
   &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
   i/o-file-does-not-exist-error?
   &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

   eof-object eof-object?
   call-with-input-file call-with-output-file 
   input-port? output-port?
   current-input-port current-output-port current-error-port
   with-input-from-file with-output-to-file
   open-input-file open-output-file
   close-input-port close-output-port 
   read-char peek-char
   read
   write-char newline 
   display write
   )

  (import
   (primitives
    &i/o make-i/o-error i/o-error?
    &i/o-read make-i/o-read-error i/o-read-error?
    &i/o-write make-i/o-write-error i/o-write-error?
    &i/o-invalid-position make-i/o-invalid-position-error
    i/o-invalid-position-error? i/o-error-position
    &i/o-filename make-i/o-filename-error i/o-filename-error?
    i/o-error-filename
    &i/o-file-protection make-i/o-file-protection-error
    i/o-file-protection-error?
    &i/o-file-is-read-only make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    &i/o-file-already-exists make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

    eof-object eof-object?
    call-with-input-file call-with-output-file 
    input-port? output-port?
    current-input-port current-output-port current-error-port
    with-input-from-file with-output-to-file
    open-input-file open-output-file
    close-input-port close-output-port 
    read-char peek-char
    read
    write-char newline 
    display write
    )))

(library (rnrs files (6))
  (export file-exists? delete-file)
  (import (primitives file-exists? delete-file)))

(library (rnrs programs (6))
  (export command-line exit)
  (import
   (core primitives)
   (rnrs base)
   (primitives
    command-line-arguments               ; [Larceny]
    exit))

  ; [Larceny]

  (define (command-line)
    (list->vector (cons 'larceny (vector->list (command-line-arguments)))))
  )

(library (rnrs arithmetic fixnums (6))

  (export
   fixnum? fixnum-width least-fixnum greatest-fixnum
   fx=? fx>? fx<? fx>=? fx<=?
   fxzero? fxpositive? fxnegative?
   fxodd? fxeven?
   fxmax fxmin
   fx+ fx- fx*
   fxdiv-and-mod fxdiv fxmod
   fxdiv0-and-mod0 fxdiv0 fxmod0
   fx+/carry fx-/carry fx*/carry
   fxnot fxand fxior fxxor
   fxif fxbit-count fxlength
   fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
   fxrotate-bit-field fxreverse-bit-field
   fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)

  (import
   (primitives
    fixnum? fixnum-width least-fixnum greatest-fixnum
    fx=? fx>? fx<? fx>=? fx<=?
    fxzero? fxpositive? fxnegative?
    fxodd? fxeven?
    fxmax fxmin
    fx+ fx- fx*
    fxdiv-and-mod fxdiv fxmod
    fxdiv0-and-mod0 fxdiv0 fxmod0
    fx+/carry fx-/carry fx*/carry
    fxnot fxand fxior fxxor
    fxif fxbit-count fxlength
    fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
    fxrotate-bit-field fxreverse-bit-field
    fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)))

(library (rnrs arithmetic flonums (6))
  
  (export
   flonum?
   real->flonum
   fl=? fl<? fl>? fl<=? fl>=?
   flinteger? flzero? flpositive? flnegative? flodd? fleven?
   flfinite? flinfinite? flnan?
   flmax flmin
   fl+ fl* fl- fl/
   flabs
   fldiv-and-mod fldiv flmod
   fldiv0-and-mod0 fldiv0 flmod0
   flnumerator fldenominator
   flfloor flceiling fltruncate flround
   flexp fllog flsin flcos fltan flasin flacos flatan
   flsqrt flexpt
   &no-infinities make-no-infinities-violation no-infinities-violation?
   &no-nans make-no-nans-violation no-nans-violation?
   fixnum->flonum)

  (import
   (primitives
    flonum?
    real->flonum
    fl=? fl<? fl>? fl<=? fl>=?
    flinteger? flzero? flpositive? flnegative? flodd? fleven?
    flfinite? flinfinite? flnan?
    flmax flmin
    fl+ fl* fl- fl/
    flabs
    fldiv-and-mod fldiv flmod
    fldiv0-and-mod0 fldiv0 flmod0
    flnumerator fldenominator
    flfloor flceiling fltruncate flround
    flexp fllog flsin flcos fltan flasin flacos flatan
    flsqrt flexpt
    &no-infinities make-no-infinities-violation no-infinities-violation?
    &no-nans make-no-nans-violation no-nans-violation?
    fixnum->flonum)))

(library (rnrs arithmetic bitwise (6))

  (export

   bitwise-not
   bitwise-and
   bitwise-ior
   bitwise-xor
   bitwise-if
   bitwise-bit-count
   bitwise-length
   bitwise-first-bit-set
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-bit-field
   bitwise-copy-bit-field
   bitwise-rotate-bit-field
   bitwise-reverse-bit-field
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right)

  (import
   (primitives

    bitwise-not
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-if
    bitwise-bit-count
    bitwise-length
    bitwise-first-bit-set
    bitwise-bit-set?
    bitwise-copy-bit
    bitwise-bit-field
    bitwise-copy-bit-field
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field)))

(library (rnrs syntax-case (6))
  
  (export make-variable-transformer
          identifier? bound-identifier=? free-identifier=?
          generate-temporaries datum->syntax syntax->datum 
          syntax-violation syntax syntax-case quasisyntax 
          unsyntax unsyntax-splicing with-syntax 
          _ ...)
  
  (import (core primitives)
          (core with-syntax)  
          (core quasisyntax))
  
  ) ;; rnrs syntax-case

(library (rnrs hashtables (6))

  (export

   make-eq-hashtable
   make-eqv-hashtable
   make-hashtable
   hashtable?
   hashtable-size
   hashtable-ref
   hashtable-set!
   hashtable-delete!
   hashtable-contains?
   hashtable-update!
   hashtable-copy
   hashtable-clear!
   hashtable-keys
   hashtable-entries
   hashtable-equivalence-function
   hashtable-hash-function
   hashtable-mutable?
   equal-hash
   string-hash
   string-ci-hash
   symbol-hash)

  (import
   (rnrs base)
   (primitives

    make-eq-hashtable
    make-eqv-hashtable
    make-r6rs-hashtable                      ; [Larceny]
    hashtable?
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-copy
    hashtable-clear!
    hashtable-keys
    hashtable-entries
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-mutable?
    equal-hash
    string-hash
    string-ci-hash
    symbol-hash))

  ; [Larceny]
  ; Larceny's traditional make-hashtable procedure is incompatible
  ; with the R6RS procedure of the same name, so the R6RS version
  ; goes under the name of make-r6rs-hashtable in R5RS mode.

  (define make-hashtable make-r6rs-hashtable))

; FIXME: (rnrs enums (6)) not yet implemented.

; FIXME: (rnrs (6)) not yet complete.

(library (rnrs (6))         
  
  (export
   
   ;; Macros defined in core expander:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   assert
   quasiquote unquote unquote-splicing
   syntax-rules identifier-syntax
   
   ;; R5RS primitives:
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char? char->integer char=? char<? char>? char<=? char>=?
   complex? cons cos 
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?
   
   ;; R6RS additional procedures:
   
   real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
   nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
   symbol=? string-for-each vector-map vector-for-each error assertion-violation
   call/cc
   
   ;; From (rnrs syntax-case)
   
   make-variable-transformer
   identifier? bound-identifier=? free-identifier=?
   generate-temporaries datum->syntax syntax->datum 
   syntax-violation syntax syntax-case quasisyntax 
   unsyntax unsyntax-splicing with-syntax 
   
   ;; From (rnrs control)
   
   when unless do case-lambda
   
   ;; From (rnrs lists)
   
   find for-all exists filter partition fold-left fold-right
   remp remove remq remv memp member memv memq
   assp assoc assv assq
   
   ;; From (rnrs io simple)
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char
   
   ;; From (rnrs unicode)
   
   char-upcase char-downcase char-titlecase char-foldcase
   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
   char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? char-title-case?
   char-general-category

   string-upcase string-downcase string-titlecase string-foldcase
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   string-normalize-nfd string-normalize-nfkd
   string-normalize-nfc string-normalize-nfkc
   
   ;; From (rnrs sorting)
   
   list-sort vector-sort vector-sort!

   ;; From (rnrs records procedural)
 
   make-record-type-descriptor record-type-descriptor?
   make-record-constructor-descriptor record-constructor
   record-predicate record-accessor record-mutator

   ;; From (rnrs records inspection)
   
   record? record-rtd record-type-name record-type-parent record-type-uid
   record-type-generative? record-type-sealed? record-type-opaque?
   record-type-field-names record-field-mutable?
   
   ;; From (rnrs arithmetic fixnums)

   fixnum? fixnum-width least-fixnum greatest-fixnum
   fx=? fx>? fx<? fx>=? fx<=?
   fxzero? fxpositive? fxnegative?
   fxodd? fxeven?
   fxmax fxmin
   fx+ fx- fx*
   fxdiv-and-mod fxdiv fxmod
   fxdiv0-and-mod0 fxdiv0 fxmod0
   fx+/carry fx-/carry fx*/carry
   fxnot fxand fxior fxxor
   fxif fxbit-count fxlength
   fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field fxcopy-bit-field
   fxrotate-bit-field fxreverse-bit-field
   fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right

   ;;; From (rnrs arithmetic flonums)

   flonum?
   real->flonum
   fl=? fl<? fl>? fl<=? fl>=?
   flinteger? flzero? flpositive? flnegative? flodd? fleven?
   flfinite? flinfinite? flnan?
   flmax flmin
   fl+ fl* fl- fl/
   flabs
   fldiv-and-mod fldiv flmod
   fldiv0-and-mod0 fldiv0 flmod0
   flnumerator fldenominator
   flfloor flceiling fltruncate flround
   flexp fllog flsin flcos fltan flasin flacos flatan
   flsqrt flexpt
   fixnum->flonum

   &no-infinities make-no-infinities-violation no-infinities-violation?
   &no-nans make-no-nans-violation no-nans-violation?

   ;; From (rnrs arithmetic bitwise)

   bitwise-not
   bitwise-and
   bitwise-ior
   bitwise-xor
   bitwise-if
   bitwise-bit-count
   bitwise-length
   bitwise-first-bit-set
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-bit-field
   bitwise-copy-bit-field
   bitwise-rotate-bit-field
   bitwise-reverse-bit-field
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right
   
   ;; From (rnrs files)
   
   file-exists? delete-file)
  
  (import (for (except (rnrs base) syntax-rules identifier-syntax _ ... set!) run expand)
          (for (only (rnrs base) set!)                                        run expand)
          (for (core syntax-rules)                                            run expand)   
          (for (core identifier-syntax)                                       run expand)
          (for (rnrs control)                                                 run expand)
          (for (rnrs lists)                                                   run expand)
          (for (rnrs syntax-case)                                             run expand)
          (for (rnrs io simple)                                               run expand)
          (for (rnrs unicode)                                                 run expand)
          (for (rnrs sorting)                                                 run expand)
          (for (rnrs records procedural)                                      run expand)
          (for (rnrs records inspection)                                      run expand)
          (for (rnrs files)                                                   run expand)
          (for (rnrs arithmetic fixnums)                                      run expand)
          (for (rnrs arithmetic flonums)                                      run expand)
          (for (rnrs arithmetic bitwise)                                      run expand)
          )
  
  ) ;; rnrs

(library (rnrs eval (6))
  (export eval environment)
  (import (core primitives)))

;; Nonstandard library for reflection on library import sets.
;; See examples file for sevaral examples.

(library (rnrs eval reflection (6))
  (export environment-bindings)
  (import (core primitives)))

(library (rnrs mutable-pairs (6))
  (export set-car! set-cdr!)
  (import (primitives set-car! set-cdr!)))

(library (rnrs mutable-strings (6))
  (export string-set! string-fill!)
  (import (primitives string-set! string-fill!)))

(library (rnrs r5rs (6))
  
  (export null-environment scheme-report-environment delay force
          exact->inexact inexact->exact quotient remainder modulo)
  
  (import (primitives exact->inexact inexact->exact quotient remainder modulo)
          (rnrs eval)
          (rnrs base)
          (rnrs control))
  
  (define (scheme-report-environment n)
    (unless (= n 5)
      (assertion-violation 'scheme-report-environment "Argument should be 5" n))
    (environment '(r5rs)))
  
  (define null-environment
    (let ((null-env
           (environment '(only (rnrs base)
                           begin if lambda quote set! and or
                           define define-syntax let-syntax letrec-syntax 
                           let let* letrec
                           case cond else =>
                           quasiquote unquote unquote-splicing
                           syntax-rules ...)
                        '(only (rnrs control) do))))
      (lambda (n)
        (unless (= n 5)
          (assertion-violation 'scheme-report-environment "Argument should be 5" n))
        null-env)))
  
  (define force
    (lambda (object)
      (object)))
  
  (define-syntax delay
    (syntax-rules ()
      ((delay expression)
       (make-promise (lambda () expression)))))
  
  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready?
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result-ready? #t)
                           (set! result x)
                           result))))))))
  ) ; rnrs r5rs

;; Nonstandard library for loading files into a
;; top-level interactive REPL environment.
;; The files may contain libraries in source form,
;; which are them dynamically loaded.  

(library (rnrs load)
  (export load)
  (import (rnrs)
          (primitives ex:repl))
  
  (define (load filename)
    (define (read-file fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p)))))))
    (ex:repl (read-file filename)))
  )
  
;; Nonstandard R5RS library:

(library (r5rs)  
  (export 
   
   ;; core primitives
   
   set!
   
   ;; rnrs base
   
   begin if lambda quote and or
   define define-syntax let-syntax letrec-syntax
   ...
   
   let let* letrec
   case cond else =>
   quasiquote unquote unquote-splicing
   syntax-rules 
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char? char->integer char=? char<? char>? char<=? char>=?
   complex? cons cos 
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?
   
   ;; rnrs eval
   
   eval
   
   ;; rnrs load
   
   load
   
   ;; rnrs control
   
   do
   
   ;; rnrs io simple
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char
   
   ;; rnrs unicode
   
   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
   string-ci<=? string-ci>=?
   
   ;; rnrs mutable pairs
   
   set-car! set-cdr!
   
   ;; rnrs lists
   
   assoc assv assq member memv memq
   
   ;; rnrs mutable-strings
   
   string-set! string-fill!
   
   ;; rnrs r5rs
   
   null-environment scheme-report-environment delay force
   exact->inexact inexact->exact quotient remainder modulo)
  
  ;; Not necessary to use only and except here, but keep
  ;; them because they contain useful information.
  
  (import (only (core primitives) set!)
          (except (rnrs base)
            set! ; because should not be exported for expand
            _ letrec* let-values let*-values identifier-syntax
            real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
            nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
            symbol=? string-for-each vector-map vector-for-each error assertion-violation
            call/cc)
          (only (rnrs eval) eval)
          (only (rnrs load) load)
          (only (rnrs control) do)
          (only (rnrs io simple)
            call-with-input-file call-with-output-file 
            close-input-port close-output-port current-input-port current-output-port
            display eof-object? newline open-input-file open-output-file peek-char
            read read-char with-input-from-file with-output-to-file write write-char)
          (only (rnrs unicode)
            char-upcase char-downcase char-ci=? char-ci<? char-ci>?
            char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
            char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
            string-ci<=? string-ci>=?)
          (only (rnrs mutable-pairs) set-car! set-cdr!)
          (only (rnrs lists) assoc assv assq member memv memq)
          (only (rnrs mutable-strings) string-set! string-fill!)
          (rnrs r5rs))
  )

;; Nonstandard explicit renaming library: 
;; See also examples and discussion in file examples.scm.
;;
;; Exports:
;;
;;    er-transformer     (syntax)
;;    bound-identifier=? (procedure)
;;    datum->syntax      (procedure)
;;
;; Differences with traditional explicit renaming:
;;
;; - The renaming procedure has signature <symbol> -> <identifier>,
;;   where the <identifier> type is disjoint from the <symbol> type.
;;
;; - The renaming procedure acts as a mathematical function in the sense that
;;   the identifiers obtained from any two calls with the same argument will
;;   be the same in the sense of bound-identifier=?, not eqv?
;;
;; - The output may not contain raw symbols, so implicit identifiers must
;;   be introduced using datum->syntax.
;;
;; - Breaking hygiene with datum->syntax allows more modular macro
;;   programming than traditional explicit renaming.
;;   See in particular the example of while in terms of loop below.
;;
;; - The renaming procedure is aware of the transformer environment,
;;   so that identifiers not bound at the usage site will resolve to
;;   the r6rs library-local bindings at the transformer site.
;;   More precisely, they will be resolved in the lexical environment
;;   of the er-transformer keyword.
;;
;; - Fully compatible with my r6rs syntax-case macro system.
;;
;; Portability and complexity note:
;;
;;   This library is not r6rs-portable, since it assumes that the input
;;   to a transformer is always an unwrapped syntax object, which is
;;   allowed but not required by r6rs, and is currently only true for my
;;   implementation.  The library could be ported to other implementations
;;   by inserting a step that unwrapped the input to the transformer.
;;   However, that would adversely modify the complexity class of
;;   er-transformer macros in those implementations.

(library (explicit-renaming helper)
  (export er-transformer)
  (import (only (rnrs) 
            define-syntax lambda syntax-case syntax datum->syntax free-identifier=?))
  
  (define-syntax er-transformer
    (lambda (exp)
      (syntax-case exp ()
        ((k proc)
         (syntax
          (lambda (form)
            (proc form
                  (lambda (symbol) (datum->syntax (syntax k) symbol))
                  free-identifier=?))))))))

(library (explicit-renaming)
  (export er-transformer identifier? bound-identifier=? datum->syntax)
  (import (explicit-renaming helper)
          (rnrs syntax-case)))

; end of file
