;;;=====================================================================
;;;
;;; Derived forms:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
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
;;; This should only be done after generate-guid in expander.scm
;;; has been suitably redefined so as to allow separate compilation. 
;;; See note on (ex:unique-token) in expander.scm. 
;;;
;;; Various of the standard macros were copied from
;;; SRFI-93 reference implementation.
;;;
;;;=====================================================================

(library (core primitives)
  
  (export
   
   ;; Macros defined in core expander:
       
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   syntax syntax-case
 
   ;; R5RS primitives:

   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer
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

   ;; For (rnrs lists) 

   find for-all exists
   filter partition fold-left fold-right
   remp remove remq remv memp member memv memq
   assp assoc assv assq

   ;; For (rnrs io simple)
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char

   ;; For (rnrs unicode)
   
   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
   string-ci<=? string-ci>=?
   
   ;; For (rnrs mutable-strings)

   string-fill! string-set!

   ;; For (rnrs mutable-lists)

   set-car! set-cdr!

   ;; For (rnrs r5rs)

   exact->inexact inexact->exact quotient remainder modulo
   
   ;; Procedures defined in core expander:

   (rename ($ex:make-variable-transformer make-variable-transformer)
           ($ex:identifier?               identifier?)
           ($ex:bound-identifier=?        bound-identifier=?)
           ($ex:free-identifier=?         free-identifier=?)
           ($ex:generate-temporaries      generate-temporaries) 
           ($ex:datum->syntax             datum->syntax)
           ($ex:syntax->datum             syntax->datum)
           ($ex:syntax-violation          syntax-violation)
           ($ex:environment               environment)
           ($ex:environment-bindings      environment-bindings)
           ($ex:eval                      eval)))
  
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

    ;; R5RS primitives:
    
    * + - / < <= = > >= abs acos append apply asin atan 
    boolean? call-with-current-continuation 
    call-with-values car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
    cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    ceiling char->integer
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

    ;; For (rnrs lists) 

   find for-all exists
   filter partition fold-left fold-right
   remp remove remq remv memp member memv memq
   assp assoc assv assq

   ;; For (rnrs io simple)
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char

   ;; For (rnrs unicode)
   
   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
   string-ci<=? string-ci>=?
   
   ;; For (rnrs mutable-strings)

   string-fill! string-set!

   ;; For (rnrs mutable-lists)

   set-car! set-cdr!

   ;; For (rnrs r5rs)
   
   exact->inexact inexact->exact quotient remainder modulo
   
   ;; Procedures defined in the core expander:
   
   $ex:make-variable-transformer $ex:identifier? $ex:bound-identifier=?
   $ex:free-identifier=? $ex:generate-temporaries $ex:datum->syntax $ex:syntax->datum 
   $ex:syntax-violation $ex:environment $ex:environment-bindings $ex:eval
   
   ))

  ) ;; core primitives

(library (core with-syntax)
  (export with-syntax)
  (import (for (core primitives) run expand))
  
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
  (import (for (core primitives)  expand run) 
          (for (core with-syntax) expand))
  
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
  (import (for (core primitives) expand run) 
          (for (core with-syntax) expand))
  
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
           (syntax (let ((i (unspecified)) ...)
                     (let ((t v) ...)
                       (set! i t) ...
                       (let () e1 e2 ...)))))))))
                       
  (define (unspecified) (if #f #f))                     
  
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
  (import (for (core primitives)   expand run) 
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand))
  
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
  (import (for (core primitives) expand run (meta -1)))  ; since generated macro contains (syntax set!) at level 0
  
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
          (for (core with-syntax) run expand))  
  
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
                         ([e* reps] (expand (syntax e) (+ level 1))))
             (syntax [(k e*) reps])))                                  
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax [t ((t e))])))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (([r* (rep ...)] (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (syntax [(t ... . r*)
                      ((t e) ... rep ...)])))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (([r* (rep ...)] (expand (syntax r) 0))
                         ((t ...)        (generate-temporaries (syntax (e ...)))))
             (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
               (syntax [(t ... ... . r*)
                        (((t ...) e) ... rep ...)]))))
          ((k . r)
           (and (> level 0)
                (identifier? (syntax k))
                (or (free-identifier=? (syntax k) (syntax unsyntax))
                    (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
           (with-syntax (([r* reps] (expand (syntax r) (- level 1))))
             (syntax [(k . r*) reps])))
          ((h . t)
           (with-syntax (([h* (rep1 ...)] (expand (syntax h) level))
                         ([t* (rep2 ...)] (expand (syntax t) level)))
             (syntax [(h* . t*)
                      (rep1 ... rep2 ...)])))
          (#(e ...)                                                               
           (with-syntax (([(e* ...) reps]
                          (expand (vector->list (syntax #(e ...))) level)))
             (syntax [#(e* ...) reps])))
          (other
           (syntax [other ()]))))
    
      (syntax-case e ()
        ((_ template)
         (with-syntax (([template* replacements] (expand (syntax template) 0)))
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
  (import (for (core primitives)  expand run) 
          (for (core let)         expand run) 
          (for (core derived)     expand run) 
          (for (core with-syntax) expand)
          (for (core quasisyntax) expand))

  ;; Optimised version copied from portable syntax-case (Dybvig)

  (define-syntax quasiquote
    (let ()
      (define (quasi p lev)
        (syntax-case p (unquote quasiquote)
          ((unquote p)
           (if (= lev 0)
               #'("value" p)
               (quasicons #'("quote" unquote) (quasi #'(p) (- lev 1)))))
          ((quasiquote p) (quasicons #'("quote" quasiquote) (quasi #'(p) (+ lev 1))))
          ((p . q)
           (syntax-case #'p (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* #'(("value" p) ...) (quasi #'q lev))
                  (quasicons
                   (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                   (quasi #'q lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend #'(("value" p) ...) (quasi #'q lev))
                  (quasicons
                   (quasicons #'("quote" unquote-splicing) (quasi #'(p ...) (- lev 1)))
                   (quasi #'q lev))))
             (_ (quasicons (quasi #'p lev) (quasi #'q lev)))))
          (#(x ...) (quasivector (vquasi #'(x ...) lev)))
          (p #'("quote" p))))
      (define (vquasi p lev)
        (syntax-case p ()
          ((p . q)
           (syntax-case #'p (unquote unquote-splicing)
             ((unquote p ...)
              (if (= lev 0)
                  (quasilist* #'(("value" p) ...) (vquasi #'q lev))
                  (quasicons
                   (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                   (vquasi #'q lev))))
             ((unquote-splicing p ...)
              (if (= lev 0)
                  (quasiappend #'(("value" p) ...) (vquasi #'q lev))
                  (quasicons
                   (quasicons
                    #'("quote" unquote-splicing)
                    (quasi #'(p ...) (- lev 1)))
                   (vquasi #'q lev))))
             (_ (quasicons (quasi #'p lev) (vquasi #'q lev)))))
          (() #'("quote" ()))))
      (define (quasicons x y)
        (with-syntax ((x x) (y y))
          (syntax-case #'y ()
            (("quote" dy)
             (syntax-case #'x ()
               (("quote" dx) #'("quote" (dx . dy)))
               (_ (if (null? #'dy) #'("list" x) #'("list*" x y)))))
            (("list" . stuff) #'("list" x . stuff))
            (("list*" . stuff) #'("list*" x . stuff))
            (_ #'("list*" x y)))))
      (define (quasiappend x y)
        (syntax-case y ()
          (("quote" ())
           (cond
            ((null? x) #'("quote" ()))
            ((null? (cdr x)) (car x))
            (else (with-syntax (((p ...) x)) #'("append" p ...)))))
          (_
           (cond
            ((null? x) y)
            (else (with-syntax (((p ...) x) (y y)) #'("append" p ... y)))))))
      (define (quasilist* x y)
        (let f ((x x))
          (if (null? x)
              y
              (quasicons (car x) (f (cdr x))))))
      (define (quasivector x)
        (syntax-case x ()
          (("quote" (x ...)) #'("quote" #(x ...)))
          (_
           (let f ((y x) (k (lambda (ls) #`("vector" #,@ls))))
             (syntax-case y ()
               (("quote" (y ...)) (k #'(("quote" y) ...)))
               (("list" y ...) (k #'(y ...)))
               (("list*" y ... z) (f #'z (lambda (ls) (k (append #'(y ...) ls)))))
               (else #`("list->vector" #,x)))))))
      (define (emit x)
        (syntax-case x ()
          (("quote" x) #''x)
          (("list" x ...) #`(list #,@(map emit #'(x ...))))
          ;; could emit list* for 3+ arguments if implementation supports list*
          (("list*" x ... y)
           (let f ((x* #'(x ...)))
             (if (null? x*)
                 (emit #'y)
                 #`(cons #,(emit (car x*)) #,(f (cdr x*))))))
          (("append" x ...) #`(append #,@(map emit #'(x ...))))
          (("vector" x ...) #`(vector #,@(map emit #'(x ...))))
          (("list->vector" x) #`(list->vector #,(emit #'x)))
          (("value" x) #'x)))
      (lambda (x)
        (syntax-case x ()
          ;; convert to intermediate language, combining introduced (but not
          ;; unquoted source) quote expressions where possible and choosing
          ;; optimal construction code otherwise, then emit Scheme code
          ;; corresponding to the intermediate language forms.
          ((_ e) (emit (quasi #'e 0)))))))
  
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
          (core let))
  
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
  
(library (rnrs control (6))
  (export when unless do case-lambda)
  (import (for (core primitives)   expand run) 
          (for (core let)          expand run)
          (for (core with-syntax)  expand)
          (for (core syntax-rules) expand))

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
       (assertion-violation #f
         "unexpected number of arguments"))
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


(library (rnrs lists (6))
  (export find for-all exists filter partition fold-left fold-right
          remp remove remq remv memp member memv memq
          assp assoc assv assq)
  (import (core primitives)))  

(library (rnrs io simple (6))
   (export call-with-input-file call-with-output-file 
           close-input-port close-output-port current-input-port current-output-port
           display eof-object? newline open-input-file open-output-file peek-char
           read read-char with-input-from-file with-output-to-file write write-char
           ;; AND SO ON
           )
   (import (core primitives)))


(library (rnrs unicode (6))
  (export char-upcase char-downcase char-ci=? char-ci<? char-ci>?
          char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
          char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
          string-ci<=? string-ci>=?
          ;; AND SO ON
          )
  (import (core primitives)))


(library (rnrs mutable-strings (6))
  (export string-set! string-fill!)
  (import (core primitives)))

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
          
(library (rnrs base (6))         
  
  (export 

   ;; Macros defined in core expander:
       
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   quasiquote unquote unquote-splicing
   syntax-rules 
   identifier-syntax
   
   ;; R5RS primitives:
     
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer
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
          (for (only (core primitives) _ ... set!) expand))
  
  ) ;; rnrs base

(library (rnrs (6))         
  
  (export

   ;; Macros defined in core expander:
       
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   quasiquote unquote unquote-splicing
   syntax-rules identifier-syntax
   
   ;; R5RS primitives:

   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer
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
   
   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
   string-ci<=? string-ci>=?
   
   ;; From (rnrs mutable-strings)
   
   string-set! string-fill!)
   
  (import (for (except (rnrs base) syntax-rules identifier-syntax _ ... set!) run expand)
          (for (only (rnrs base) set!)                                        run expand)
          (for (core syntax-rules)                                            run expand)   
          (for (core identifier-syntax)                                       run expand)
          (for (rnrs control)                                                 run expand)
          (for (rnrs lists)                                                   run expand)
          (for (rnrs syntax-case)                                             run expand)
          (for (rnrs io simple)                                               run expand)
          (for (rnrs unicode)                                                 run expand)
          (for (rnrs mutable-strings)                                         run expand))
            
  ) ;; rnrs

(library (rnrs eval (6))
  (export eval environment)
  (import (core primitives)))

(library (rnrs mutable-pairs (6))
  (export set-car! set-cdr!)
  (import (core primitives)))

(library (rnrs r5rs (6))
         
  (export null-environment scheme-report-environment delay force
          exact->inexact inexact->exact quotient remainder modulo)

  (import (core primitives) 
          (rnrs eval)
          (rnrs base)
          (rnrs control))

  (define scheme-report-environment
    (let ((r5rs-env 
           (environment
            '(except (rnrs base)
                     _ letrec* let-values let*-values
                     real-valued? rational-valued? integer-valued? exact inexact finite? infinite?
                     nan? div mod div-and-mod div0 mod0 div0-and-mod0 exact-integer-sqrt boolean=?
                     symbol=? string-for-each vector-map vector-for-each error assertion-violation
                     call/cc)
            '(only (rnrs eval) eval)
            '(only (rnrs control) do)
            '(only (rnrs lists) assoc assv assq)
            '(only (rnrs io simple)
                   call-with-input-file call-with-output-file 
                   close-input-port close-output-port current-input-port current-output-port
                   display eof-object? newline open-input-file open-output-file peek-char
                   read read-char with-input-from-file with-output-to-file write write-char)
            '(only (rnrs unicode)
                   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
                   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
                   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
                   string-ci<=? string-ci>=?)
            '(only (rnrs mutable-pairs) set-car! set-cdr!)
            '(only (rnrs lists) assoc assv assq member memv memq)
            '(only (rnrs mutable-strings) string-set! string-fill!)
            '(rnrs r5rs))))
      (lambda (n)
        (unless (= n 5)
          (assertion-violation 'scheme-report-environment "Argument should be 5" n))
        r5rs-env)))

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

;; Nonstandard library for reflection

(library (rnrs eval reflection (6))
  (export environment-bindings)
  (import (core primitives)))

                    
  
