;;;=====================================================================
;;;
;;; Derived forms:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;   January 15, 2007 
;;;
;;;=====================================================================

;;;=====================================================================
;;;
;;; This file builds r6rs up using a sequence of libraries.
;;; It constitutes a nontrivial example, tutorial and test
;;; of the library system.  
;;;
;;; It is meant to be expanded by macros-core and compiled 
;;; together with the latter before using in a production system.
;;; This should only be done after generate-guid in macros-core.scm
;;; has been suitably redefined so as to allow separate compilation.  
;;; See note at end of macros-core.
;;;
;;; Various of the standard macros were copied from
;;; SRFI-93 reference implementation.
;;;
;;;=====================================================================

(library (core primitives)
  
  (export
   
   ;; Primitive procedures:
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation call-with-input-file
   call-with-output-file call-with-values call/cc car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
   char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase
   char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char?
   close-input-port close-output-port complex? cons cos current-input-port
   current-output-port denominator display dynamic-wind eof-object?
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? input-port? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? newline not null? number->string number? numerator
   odd? open-input-file open-output-file output-port? pair? peek-char port?
   positive? procedure? rational? rationalize read
   read-char real-part real? reverse round
   set-car! set-cdr! sin sqrt string string->list string->number string->symbol
   string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
   string-copy string-fill! string-length string-ref string-set! string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   transcript-off transcript-on truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? with-input-from-file
   with-output-to-file write write-char zero?
   
   ;; R6RS additional procedures:
   
   unspecified error contract-violation 
   for-all filter partition fold-left fold-right
   remp remove remq remv memp memv member memq
   assp assoc assv assq
                       
   ;; Procedures defined in core expander:
   
   (rename ($make-variable-transformer make-variable-transformer)
           ($identifier?               identifier?)
           ($bound-identifier=?        bound-identifier=?)
           ($free-identifier=?         free-identifier=?)
           ($generate-temporaries      generate-temporaries) 
           ($datum->syntax             datum->syntax)
           ($syntax->datum             syntax->datum)
           ($syntax-violation          syntax-violation)                 
           ($environment               environment)
           ($r6rs-eval                 eval)
           ($r6rs-load                 load)) 
   
   ;; Macros defined in the core expander:
   
   begin if set! and or lambda quote
   define define-syntax let-syntax letrec-syntax  
   syntax syntax-case _ ...
   declare unsafe safe fast small debug)
  
  (import
   
   ;; An extension to the r6rs import syntax, used here to make  
   ;; available the Scheme primitive procedures, as well as the 
   ;; appropriate macros and procedures defined already in 
   ;; the core expander.  This is the only place it is used.  
   
   (primitives
    
    ;; Primitive procedures: 
    
    (* + - / < <= = > >= abs acos append apply asin assoc assq assv atan 
       boolean? call-with-current-continuation call-with-input-file
       call-with-output-file call-with-values call/cc car cdr caar cadr cdar cddr
       caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
       cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
       ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
       char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase
       char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char?
       close-input-port close-output-port complex? cons cos current-input-port
       current-output-port denominator display dynamic-wind eof-object?
       eq? equal? eqv? even? exact? exp expt floor for-each
       gcd imag-part inexact? input-port? integer->char integer?
       lcm length list list->string
       list->vector list-ref list-tail list? log magnitude make-polar
       make-rectangular make-string make-vector map max member memq memv min
       negative? newline not null? number->string number? numerator
       odd? open-input-file open-output-file output-port? pair? peek-char port?
       positive? procedure? rational? rationalize read
       read-char real-part real? reverse round
       set-car! set-cdr! sin sqrt string string->list string->number string->symbol
       string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
       string-copy string-fill! string-length string-ref string-set! string<=? string<?
       string=? string>=? string>? string? substring symbol->string symbol? tan
       transcript-off transcript-on truncate values vector vector->list
       vector-fill! vector-length vector-ref vector-set! vector? with-input-from-file
       with-output-to-file write write-char zero?
       
       ;; R6RS additional primitives:
       
       unspecified error contract-violation
       for-all filter partition fold-left fold-right
       remp remove remq remv memp memv member memq
       assp assoc assv assq
       
       ;; Procedures defined in the core expander:
       
       $make-variable-transformer $identifier? $bound-identifier=?
       $free-identifier=? $generate-temporaries $datum->syntax $syntax->datum 
       $syntax-violation $environment $r6rs-eval $r6rs-load
       
       ;; Macros defined in core expander:
       
       begin if set! and or lambda quote
       define define-syntax let-syntax letrec-syntax 
       syntax syntax-case _ ... 
       declare unsafe safe fast small debug)))
   
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
  (import (for (core primitives)  expand run) 
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
  
  (define-syntax letrec*
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (syntax (let ()
                   (define i v) ...
                   (let () e1 e2 ...)))))))
  
  ) ; let

(library (core derived)
  (export let* cond case do else =>)   
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
;;; (quasisyntax (set! ,a ,b))
;;;   ==> (with-syntax ((t0 a)
;;;                     (t1 b))
;;;         (syntax (set! t0 t1)))
;;;
;;; (quasisyntax (list ,@args))
;;;   ==> (with-syntax (((t ...) args))
;;;         (syntax (list t ...)))
;;;
;;; Note that quasisyntax is expanded first, before any
;;; ellipses act.  For example:
;;;
;;; (quasisyntax (f ((b ,a) ...))
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
;;;                      (quote ((b ,a) ...))))))))
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
        (with-syntax ((::: (datum->syntax (syntax here) '...)))
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
               (with-syntax ((((t ...) ...) (syntax ((t :::) ...))))
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
             (syntax [other ()])))))
      
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
  
  ;; Unoptimized.  See Dybvig source for optimized version.
  
  (define-syntax quasiquote
    (lambda (s)
      (define (qq-expand x level)
        (syntax-case x (quasiquote unquote unquote-splicing)
          (`x                             (quasisyntax (list 'quasiquote
                                                             #,(qq-expand (syntax x) (+ level 1)))))
          (,x (> level 0)                 (quasisyntax (cons 'unquote
                                                             #,(qq-expand (syntax x) (- level 1)))))
          (,@x (> level 0)                (quasisyntax (cons 'unquote-splicing
                                                             #,(qq-expand (syntax x) (- level 1)))))
          (,x (= level 0)                 x)
          
          (((unquote x ...) . y)
           (= level 0)                    (quasisyntax (append (list x ...)
                                                               #,(qq-expand (syntax y) 0))))
          (((unquote-splicing x ...) . y)
           (= level 0)                    (quasisyntax (append (append x ...)
                                                               #,(qq-expand (syntax y) 0))))
          ((x . y)                        (quasisyntax (cons  #,(qq-expand (syntax x) level)
                                                              #,(qq-expand (syntax y) level))))
          (#(x ...)                       (quasisyntax (list->vector #,(qq-expand (syntax (x ...))    
                                                                                  level))))
          (x  (syntax 'x)))) 
      (syntax-case s ()
        ((_ x) (qq-expand (syntax x) 0)))))
  
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

(library (r6rs lists)
  (export find for-all exists filter partition fold-left fold-right
          remp remove remq remv memp member memv memq
          assp assoc assv assq)
  (import (core primitives)  
          (core derived))
  
  ;; Define here any of the above that is not already primitive.
  ;; These are only example implementations that do not have the 
  ;; full r6rs semantics, but are sufficient for the records 
  ;; library demo.
  
  (define (exists f l)
    (cond
      ((null? l) #f)
      ((null? (cdr l)) (f (car l)))
      ((f (car l)) => (lambda (v) v))
      (else (exists f (cdr l)))))
  
  (define (find proc ls)
    (cond ((null? ls) #f)
          ((proc (car ls)) (car ls))
          (else (find proc (cdr ls)))))
  
  ;; and so on ...
  )

(library (r6rs syntax-case)
  
  (export make-variable-transformer
          identifier? bound-identifier=? free-identifier=?
          generate-temporaries datum->syntax syntax->datum 
          syntax-violation syntax syntax-case quasisyntax 
          unsyntax unsyntax-splicing with-syntax 
          _ ...)
  
  (import (core primitives)
          (core with-syntax)  
          (core quasisyntax))
  
  ) ;; r6rs syntax-case
          
(library (r6rs base)         
  
  (export 
   
   ;; Primitive procedures:
     
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation call-with-input-file
   call-with-output-file call-with-values call/cc car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
   char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase
   char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char?
   close-input-port close-output-port complex? cons cos current-input-port
   current-output-port denominator display dynamic-wind eof-object?
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? input-port? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? newline not null? number->string number? numerator
   odd? open-input-file open-output-file output-port? pair? peek-char port?
   positive? procedure? rational? rationalize read
   read-char real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
   string-copy string-fill! string-length string-ref string-set! string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   transcript-off transcript-on truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? with-input-from-file 
   with-output-to-file write write-char zero?
   
   ;; R6RS additional procedures:
   
   unspecified error contract-violation 
   
   ;; Macros defined in core expander:
       
   begin if set! and or lambda quote
   define define-syntax let-syntax letrec-syntax 
   declare unsafe safe fast small debug
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond do else =>
   quasiquote unquote unquote-splicing
   
   syntax-rules 
   identifier-syntax)
  
  (import (core primitives)       
          (core let)                          
          (core derived)             
          (core quasiquote)        
          (core let-values)
          (for (core syntax-rules)      expand)   
          (for (core identifier-syntax) expand)
          (for (only (core primitives) _ ...) expand))
  
  ) ;; r6rs base

(library (r6rs)         
  
  (export 
   
   ;; R5RS primitives:
     
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation call-with-input-file
   call-with-output-file call-with-values call/cc car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
   cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
   char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase
   char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char?
   close-input-port close-output-port complex? cons cos current-input-port
   current-output-port denominator display dynamic-wind eof-object?
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? input-port? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? newline not null? number->string number? numerator
   odd? open-input-file open-output-file output-port? pair? peek-char port?
   positive? procedure? rational? rationalize read
   read-char real-part real? reverse round 
   sin sqrt string string->list string->number string->symbol
   string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
   string-copy string-fill! string-length string-ref string-set! string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   transcript-off transcript-on truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? with-input-from-file 
   with-output-to-file write write-char zero?
   
   ;; R6RS additional procedures:
   
   unspecified error contract-violation 
   
   ;; Lists library
   
   find for-all exists filter partition fold-left fold-right
   remp remove remq remv memp member memv memq
   assp assoc assv assq
   
   ;; Syntax-case library
   
   make-variable-transformer
   identifier? bound-identifier=? free-identifier=?
   generate-temporaries datum->syntax syntax->datum 
   syntax-violation syntax syntax-case syntax-rules quasisyntax 
   unsyntax unsyntax-splicing with-syntax identifier-syntax
   _ ... 
   
   ;; Macros defined in core expander:
       
   begin if set! and or lambda quote
   define define-syntax let-syntax letrec-syntax 
   declare unsafe safe fast small debug
   
   ;; Derived syntax:
   
   let let* letrec letrec* let-values let*-values
   case cond do else =>
   quasiquote unquote unquote-splicing)
  
  (import (for (except (r6rs base) syntax-rules identifier-syntax _ ...) run expand)
          (for (only (core primitives) ... _)                            run expand)
          (for (r6rs lists)                                              run expand)
          (for (r6rs syntax-case)                                        run expand)
          (for (core syntax-rules)                                       run expand)   
          (for (core identifier-syntax)                                  run expand))
  
  ) ;; r6rs

(library (r6rs eval)
  (export eval environment)
  (import (core primitives)))

(library (r6rs mutable-pairs)
  (export set-car! set-cdr!)
  (import (core primitives)))

