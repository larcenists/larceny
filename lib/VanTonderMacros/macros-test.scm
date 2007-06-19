;;;=====================================================================
;;;
;;; Tests:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html 
;;;
;;;   January 15, 2007   
;;; 
;;;=====================================================================
 
(load "macros-core")   

($repl
 '( 
   ;; Make r6rs available at toplevel.
   
   (import (r6rs)) 
  
   ;;====================================================================
   ;;
   ;; Correct lexical scoping of expansion algorithm: 
   ;;
   ;; Examples where and error *must* be thrown to avoid giving a 
   ;; lexical-scope-violating semantics to expressions: 
   ;;
   ;;====================================================================
   
   ;; This must give an error:
   
   ;; (let ()
   ;;   (define-syntax foo (lambda (e) (+ 1 2)))
   ;;   (define + 2)   
   ;;   (foo))        ; Syntax violation: Redefinition of identifier + that 
   ;;                                     has already been referenced during expansion of body
   
   ;; This gives no error:
   
   (let ()
     (define-syntax foo (lambda (e) (let ((+ -)) (+ 1 2))))
     (define + 2)
     (foo))           ;==> -1
  
   ;; This must give an error:
 
   ;; (program 
   ;;  (import (r6rs))
   ;;  (bar x)
   ;;  (define-syntax bar     
   ;;    (syntax-rules ()
   ;;      ((bar x) (define x 1))))
   ;;  x)                  ; Syntax violation: Redefinition of identifier bar that has
   ;;                                          already been referenced during expansion of body
   
   ;;(let ((x #f))
   ;;  (let-syntax ((foo (syntax-rules (x)
   ;;                      ((_ x y) (define y 'outer))
   ;;                      ((_ _ y) (define y 'inner)))))
   ;;    (let ()
   ;;      (foo x p)
   ;;      (define x #f)
   ;;      p)))            ; Syntax violation: Redefinition of identifier x that has 
   ;;                                          already been referenced during expansion of body
   
   ;; Still, the following is valid.
   
   (let ((x #f))
     (let-syntax ((foo (syntax-rules (x)
                         ((_ x y) (define y 'outer))
                         ((_ _ y) (define y 'inner)))))
       (let ()
         (define x #f)
         (foo x p)
         p)))              ;==> inner    
   
   ;;(let ((x #f))
   ;;  (let-syntax ((foo (syntax-rules (x)
   ;;                      ((_ x y) (define y 'outer))
   ;;                      ((_ _ y) 1))))
   ;;    (let ()
   ;;      (foo x p)
   ;;      (define x #f)
   ;;      p)))            ; Syntax violation: Redefinition of identifier x that has 
   ;;                                          already been referenced during expansion of body
   
   ;;(let-syntax ([def0 (syntax-rules ()
   ;;                     [(_ x) (define x 0)])])
   ;;  (let ()
   ;;    (def0 z)
   ;;    (define def0 '(def 0))
   ;;    (list z def0)))   ; Syntax violation: Redefinition of identifier def0 that has 
   ;;                                          already been referenced during expansion of body
   
   ;;(let ()
   ;;  (define define 17)
   ;;  define)             ; Syntax violation: Redefinition of identifier define that has 
   ;;                                          already been referenced during expansion of body
   
   ;; (define-syntax foo (syntax-rules () ((_ x) (define x 1))))
   ;;   (let ((b 2))
   ;;     (foo a)
   ;;     (define (foo x) 2)
   ;;     (foo b)
   ;;     (values a b))   ;==> Syntax violation: Redefinition of identifier foo that has
   ;;                     ;                      already been referenced during expansion of body
   
   ;; (define-syntax foo (syntax-rules () ((_ x) (define x 1))))
   ;;   (let ()
   ;;     (foo a)
   ;;     (define-syntax foo (syntax-rules () ((_ x) (define x 2))))
   ;;     (foo b)
   ;;     (values a b))   ;==> Syntax violation: Redefinition of identifier foo that has  
   ;;                     ;                      already been referenced during expansion of body

   ;; This should still be valid.
   
   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((_ def0) (def0 define 17))))
     (foo define)
     0)
   
   ;;;=====================================================================
   ;;;
   ;;; LIBRARIES AND PROGRAMS:
   ;;;
   ;;; The file macros-derived.scm builds r6rs up using a sequence 
   ;;; of r6rs libraries.  It constitutes a nontrivial example, 
   ;;; tutorial and test of the library system.  
   ;;; 
   ;;; Here are some further tests and examples:
   ;;;
   ;;;=====================================================================
   
   ;;;====================================================
   ;;;
   ;;; R6RS library examples.
   ;;;
   ;;;====================================================

   (library (stack)
     (export make push! pop! empty!)
     (import (r6rs)
             (r6rs mutable-pairs))
     
     (define (make)
       (list '()))
     
     (define (push! s v)
       (set-car! s (cons v (car s))))
     
     (define (pop! s)
       (let ((v (caar s))) (set-car! s (cdar s)) v))
     
     (define (empty! s)
       (set-car! s '()))
     )
   
   (library (balloons)
     (export make push pop)
     (import (r6rs))
     
     (define (make w h)
       (cons w h))
     
     (define (push b amt)
       (cons (- (car b) amt) (+ (cdr b) amt)))
     
     (define (pop b)
       (display "Boom! ")
       (display (* (car b) (cdr b)))
       (newline))
     )
   
   (library (party)
     (export (rename (balloon:make make) (balloon:push push))
             push! make-party
             (rename (party-pop! pop!)))
     (import (r6rs)
             (only (stack) make push! pop!) ;; not empty!
             (prefix (balloons) balloon:))
     
     ;; Creates a party as a stack of balloons, starting with
     ;; two balloons
     (define (make-party)
       (let ((s (make))) ;; from stack
         (push! s (balloon:make 10 10))
         (push! s (balloon:make 12 9)) s))
     
     (define (party-pop! p)
       (balloon:pop (pop! p)))
     )
   
   (library (main)
     (export)
     (import (r6rs) (party))
     (define p (make-party))
     (pop! p)                      ;; displays "Boom! 108"
     (push! p (push (make 5 5) 1))
     (pop! p))                     ;; displays "Boom! 24"
   
   ;; This completes the program, executing main:
   
   (program
    (import (main))
    0)
   
   ;; In the current implementation, one can also do
   ;; this to execute a library:
   
   (import (main))
  
   ;; Macros and meta-levels
   
   (library (my-helpers id-stuff)
     (export find-dup)
     (import (r6rs))
     
     (define (find-dup l)
       (and (pair? l)
            (let loop ((rest (cdr l)))
              (cond ((null? rest)
                     (find-dup (cdr l)))
                    ((bound-identifier=? (car l) (car rest))
                     (car rest))
                    (else (loop (cdr rest)))))))
     )
   
   (library (my-helpers value-stuff)
     (export mvlet)
     (import (r6rs)
             (for (my-helpers id-stuff) expand))
     
     (define-syntax mvlet
       (lambda (stx)
         (syntax-case stx ()
           ((_ ((id ...) expr) body0 body ...)
            (not (find-dup (syntax (id ...))))
            (syntax
             (call-with-values
              (lambda () expr)
              (lambda (id ...) body0 body ...))))))))
   
   (library (let-div)
     (export let-div)
     (import (r6rs) (my-helpers value-stuff))
     
     (define (quotient+remainder n d)
       (let ((q (floor (/ n d))))
         (values q (- n (* q d)))))   
     
     (define-syntax let-div
       (syntax-rules ()
         ((_ n d (q r) body0 body ...)
          (mvlet ((q r) (quotient+remainder n d))
                 body0 body ...))))
     )
  
   (program
    (import (let-div) (r6rs))   
    (let-div 5 2 (q r) (+ q r))  ;==> 3
    )

   ;;======================================================
   ;;
   ;; Further program tests:
   ;;
   ;;======================================================
     
   ;; Test expressions returning no values and more than one value
   ;; preceding definitions:
   
   (program
    (import (r6rs))
    (define x 1)
    (set! x 2)
    (values)
    (display 4)          ;==> 4
    (values 2 3)
    (define y 3)
    (+ x y))             ;==> 5
      
   ;;======================================================
   ;;
   ;; Further library tests:
   ;;
   ;;======================================================
   
   ;; Test meta-level resolution for chained imports:
   
   (library (foo) 
     (export u)
     (import (r6rs))
     (define u 1))
   
   (library (bar)
     (export u v)
     (import (r6rs) (foo))
     (define-syntax v (lambda (e) (syntax u))))
   
   (library (baz)
     (export)
     (import (for (r6rs) (meta 2) expand run) 
             (for (bar)  (meta 2)))
     (display 
      (let-syntax ((m (lambda (e)
                        (let-syntax ((n (lambda (e) (+ u (v)))))
                          (n)))))
        (m))))                       

   (import (baz))    ;==> 2 
  
   ;;======================================================
   ;;
   ;; Check that export levels compose correctly:
   ;;
   ;;======================================================
   
   (library (foo)
     (export x y)    
     (import (r6rs))
     (define x 2)
     (define y 4))
   
   (library (baz)
     (export y)                      ;; exports y at level 1
     (import (r6rs) (for (foo) expand)))
   
   (library (bar)
     (export f)
     (import (for (r6rs) expand run)   ;; This also implicitly imports into (meta 2) 
             (for (foo)  expand)       ;; imports x and y at level 1
             (for (baz)  expand))      ;; also imports y but at level expand + 1 = 2
     (define (f) 
       (let-syntax ((foo (lambda (_) 
                           (+ x                                   ;; level 1
                              y                                   ;; level 1 
                              (let-syntax ((bar (lambda (_) y)))  ;; level 2
                                (bar))))))
         (foo))))
   
   (import (bar))
   (f)   ;==> 10   
   
   ;;==========================================================
   ;;
   ;; Check that levels of reference are determined lexically:
   ;;
   ;;==========================================================
   
   (library (foo)  
     (export f) 
     (import (r6rs))
     (define (f) 1)) 
 
   (library (bar) 
     (export g) 
     (import (r6rs) 
             (for (foo) expand))  ;; This is the wrong level !
     (define-syntax g 
       (syntax-rules () 
         ((_) (f))))) 
 
   ;; This *must* be an error:
   ;; The use of f in bar cannot be satisfied
   ;; by the import of foo into the client level 0 here. 
   ;; That would violate lexical determination of
   ;; level of reference to f in bar. 
   
   ;; (library (main) 
   ;;   (export)   
   ;;   (import (r6rs) (foo) (bar))   
   ;;   (display (g)))         
   
   ;; ==> Syntax violation: Attempt to use binding of f at invalid meta level 0. 
   ;;     Binding is only valid at meta levels: 1 
   
   ;; Example from http://www.r6rs.org/r6rs-editors/2006-August/001682.html
   
   (library (A)
     (export x)
     (import (r6rs))
     (define x 37))
   
   (library (B)
     (export)
     (import (A)))
   
   (library (C)
     (export foo)
     (import (r6rs) (for (A) expand))
     (define-syntax foo
       (syntax-rules ()
         ((_) x))))
   
   (library (D)
     (export foo)
     (import (r6rs) (C)))
   
   ;; This has to raise syntax error to avoid problem described in 
   ;; above message.
   
   (library (E)
     (export)
     (import (r6rs) (B) (D))
     ;; (display (foo))  ; Attempt to use x at invalid meta level 0.  
     ;;                  ; Binding is only available at meta levels: 1 
     )
  
   ;;==============================================================
   ;;
   ;; Importing into multiple and negative levels: 
   ;;
   ;;==============================================================
   
   (library (foo) 
     (export x)
     (import (r6rs))
     (define x 42))
   
   (library (bar)
     (export get-x)
     (import (r6rs)
             ;; Code in (syntax ...) expressions refer to bindings
             ;; at one lower level - for example, ordinary macros
             ;; are evaluated at level expand = 1, but manipulate 
             ;; code that will run at level run = 0.
             ;; The occurrence of (syntax x) below is not in a macro
             ;; but rather at level 0.
             ;; The reference x in (syntax x) is therefore at level -1.
             ;; To make it refer to the x in foo, we need to import 
             ;; the latter at level -1.
             (for (foo) (meta -1)))
     (define (get-x) (syntax x)))
   
   (library (baz)
     (export)
     (import (for (r6rs) (meta 3) (meta 2) expand run) 
             (for (bar)  (meta 3) expand))
     
     (display
      (let-syntax ((m (lambda (ignore)
                        (get-x))))
        (m)))                                    ;==> 42
     
     (display 
      (let-syntax ((m (lambda (ignore)
                        (let-syntax ((n (lambda (ignore)
                                          (let-syntax ((o (lambda (ignore)
                                                            (get-x))))
                                            (o)))))
                          (n)))))
        (m)))                                    ;==> 42
     
     ;; This should give a syntax error due to a displaced reference:
     
     ;; (display 
     ;;  (let-syntax ((m (lambda (ignore)
     ;;                     (let-syntax ((n (lambda (ignore)
     ;;                                       (get-x))))
     ;;                       (n)))))
     ;;    (m)))              ;==> Syntax-violation: Attempt to use binding of get-x at invalid level 2 
                            ;                      Binding is only valid at levels (1 3) 
     
     ) ;; baz
   
   (import (baz))   ;==> 42 42
   
   ;;=============================================================
   ;;
   ;; Nice practical example of negative levels:
   ;;
   ;;=============================================================
      
   (library (print)
     (export print-args)
     (import (r6rs))
     (define print-args
       (lambda (fml* act*)
         (display "Lambda ")
         (display fml*)
         (display " : ")
         (display act*)
         (newline))))
   
   (library (tracers-helpers)
     (export trace-transformer untrace-transformer)
     (import (for (r6rs)  (meta -1) run) 
             (for (print) (meta -1)))
     (define trace-transformer
       (lambda (stx)
         (syntax-case stx ()
           [(_ fml* b b* ...)
            #'(lambda act*
                (print-args 'fml* act*)
                (apply (lambda fml* b b* ...) act*))])))
     (define untrace-transformer
       (lambda (stx)
         (syntax-case stx ()
           [(_ fml* b b* ...)
            #'(lambda fml* b b* ...)]))))
     
   (library (tracers)
     (export trace-region untrace-region)
     (import (r6rs)
             (for (tracers-helpers) expand))
   
     (define-syntax trace-region
       (lambda (x)
         (syntax-case x ()
           [(kwd b b* ...)
            (with-syntax ([L (datum->syntax #'kwd 'lambda)])
              #'(let-syntax ([L trace-transformer])
                  b b* ...))])))
     
     (define-syntax untrace-region
       (lambda (x)
         (syntax-case x ()
           [(kwd b b* ...)
            (with-syntax ([L (datum->syntax #'kwd 'lambda)])
              #'(let-syntax ([L untrace-transformer])
                  b b* ...))]))))
   
   (library (FOO)
     (export)
     (import (r6rs) (tracers))
     (define a (lambda (q) (display "A not traced\n")))
     (trace-region
      (define b (lambda (r) (display "did it work in B?\n")))
      (untrace-region
       (define c (lambda (s) (display "C not traced\n"))))
      (define d (lambda (t) (display "did it work in D?\n"))))
     (a 'a)
     (b 'b)
     (c 'c)
     (d 'd))
   
   (import (FOO))
   ;;   ==> A not traced
   ;;       Lambda (r) : (b)
   ;;       did it work in B?
   ;;       C not traced
   ;;       Lambda (t) : (d)
   ;;       did it work in D?
   
   ;;====================================================================
   ;;
   ;; SHARING OF BINDINGS:
   ;;
   ;; The following detects whether bindings are shared between
   ;; different meta levels that may be present at expansion time.  
   ;;
   ;;====================================================================
   
   (library (foo) 
     (export counter)
     (import (r6rs))
     (define counter
       (let ((x 0)) 
         (lambda () 
           (set! x (+ x 1))
           x))))
   
   (library (bar)
     (export)
     (import (r6rs) 
             (for (foo) run expand))
     (let-syntax ((m (lambda (e) (counter))))
       (display (list (m) (counter)))))
  
   (import (bar))    ;==> (1 1) since bindings are never shared between expand and run
  
   (library (baz)
     (export)
     (import (for (r6rs) run expand)
             (for (foo) expand (meta 2)))
     (let-syntax ((_ (let-syntax ((m (lambda (e) (counter))))
                       (display (list (m) (counter)))
                       (lambda (_) _))))))
   
   ;;                             ;==>  (1 1) when bindings are not shared at expand-time
   ;;                                   (1 2) when bindings are shared at expand-time
  
   ;; This detects whether macro bindings are shared between levels.
   
   (library (foo)
     (export f)
     (import (r6rs))
     (define f
       (let ((x 0)) 
         (lambda () 
           (set! x (+ x 1))
           x))))
   
   (library (bar)
     (export m)
     (import (r6rs) (for (foo) expand))
     (define-syntax m
       (lambda (e)
         (f))))
   
   (library (baz)
     (export)
     (import (for (r6rs) run expand) 
             (for (bar) expand (meta 2)))
     (let-syntax ((n (lambda (e) 
                       (let-syntax ((o (lambda (e) (m))))
                         (+ (m) (o))))))
       (display (n))))
   
   (import (baz))  ;==> 2 if macro bindings are not shared 
   ;;                   3 if macro bindings are shared
   
   ;;====================================================================
   ;;
   ;; Levels are not identical to phases:
   ;;
   ;; Example illustrating that two identifier references (1) and (2)
   ;; both at level 1 can be evaluated in very different phases.  
   ;; Reference (1) is not evaluated at all during expansion of (foo),
   ;; but only later during expansion of (bar). 
   ;;
   ;;====================================================================
   
   (library (baz)
     (export x)
     (import (r6rs))
     (define x 1))
   
   (library (foo)
     (export template)
     (import (for (baz) expand)
             (for (r6rs) run (meta -1)))
     (define (template)
       (syntax (let-syntax ((m (lambda (_) 
                                 (let-syntax ((n (lambda (_) x)))  ; <== (1)
                                   (n)))))
                 (m))))
     (let-syntax ((n (lambda (_) x)))  ; <== (2)
       (n)))
   
   (library (bar)
     (export)
     (import (r6rs)
             (for (foo) expand))
     (let-syntax ((n (lambda (_) (template))))
       (display (n)))
     )
     
   (import (bar))
   
   ;;================================================================
   ;;
   ;; Level enforcement while visiting or invoking libraries:
   ;;
   ;; The following test checks that level enforcing works correctly
   ;; also for expand-time toplevel identifiers created while
   ;; visiting libraries.
   ;;
   ;;================================================================
   
   (library (D)
     (export put! get)
     (import (r6rs))
     (define v #f)
     (define (put! x) (set! v x))
     (define (get) v))
   
   (library (B)
     (export b)
     (import (r6rs))
     (define b 7))
   
   (library (A)
     (export do-a)
     (import (r6rs)
             (for (D) expand)
             (for (B) run))  
     
     (define-syntax do-a
       (begin (put! (syntax b))
              (lambda (stx)
                #f))))
   
   (library (C)
     (export)
     (import (for (r6rs) run expand)
             (for (D) expand)
             
             ;; In this test, change RUN to EXPAND.  The following syntax
             ;; error should be obtained:
             ;;
             ;; Syntax violation: invalid reference
             ;; Attempt to use b at invalid meta level -1.  
             ;; Binding is only available at meta levels: 0 
             
             (for (A) run))
     
     (define-syntax make-ref
       (lambda (stx)
         (get)))
   
     (display (make-ref)))
   
             
   
   (import (C))
   
   ;;=============================================================
   ;;
   ;; Behaviour of free-identifier=? (technical):
   ;;
   ;; This implementation has (free-identifier=? x y) = #t  
   ;; if x and y would be equivalent as references.  
   ;; In foo1, the cons in (syntax (cons 1 2)) is at level -1
   ;; and therefore does not refer to the r6rs cons, which is
   ;; imported at level 0.  In foo2, it does refer to the
   ;; r6rs cons.
   ;;
   ;;=============================================================
   
   (library (foo1)
     (export helper1)
     (import (r6rs))
     
     (define (helper1 e) 
       (syntax-case e ()
         ((_ k) 
          (begin
            (if (free-identifier=? (syntax k) (syntax cons))
                (begin (display 'true)
                       (syntax (cons 1 2)))
                (begin (display 'false)
                       (syntax (k 1 2)))))))))
   
   (library (foo2)
     (export helper2)
     (import (for (r6rs) (meta -1) run))
     
     (define (helper2 e) 
       (syntax-case e ()
         ((_ k) 
          (begin
            (if (free-identifier=? (syntax k) (syntax cons))
                (begin (display 'true)
                       (syntax (cons 1 2)))
                (begin (display 'false)
                       (syntax (k 1 2)))))))))
       
   (library (bar)
     (export test1 test2)
     (import (r6rs) 
             (for (foo1) expand)
             (for (foo2) expand))    
     
     (define-syntax test1
       (lambda (stx)
         (helper1 stx)))
     
     (define-syntax test2
       (lambda (stx)
         (helper2 stx))))
   
   (import (bar))
   
   (test1 cons)   ;==> false (1 . 2)
     
   (test2 cons)   ;==> true  (1 . 2)
     
   ;;======================================================
   ;;
   ;; Example from Matthew Flatt's paper.
   ;; Illustrates use of expand-time state.
   ;;
   ;;====================================================== 
    
   (library (records-helper)
     (export register! registered?)
     (import (r6rs))
     (define table '())
     (define (register! name)
       (set! table (cons name table)))
     (define (registered? name)
       (memp (lambda (entry) (free-identifier=? name entry))
             table)))
     
   (library (records) 
     (export define-record record-switch)
     (import (r6rs) (for (records-helper) expand))
     (define-syntax define-record
       (lambda (form)
         (syntax-case form ()
           ((_ name) 
            (syntax 
             (begin 
               (define name 'record-type-deprogramor)
               (define-syntax dummy 
                 (begin 
                   (register! (syntax name))
                   (lambda (form) 'never-used)))))))))
     (define-syntax record-switch
       (lambda (form)
         (syntax-case form ()
           ((_ exp (name consequence))
            (if (registered? (syntax name))
                (syntax (if (eq? exp 'name) consequence "no match"))
                (syntax-violation #f "Invalid record type" (syntax name))))))))
   
   (library (zoo)
     (export zebra)
     (import (records))
     (define-record zebra))
   
   (library (metrics)
     (export)
     (import (r6rs) (zoo) (records))
     (display 
      (record-switch 'zebra (zebra 'zebra))))   ;==> zebra
   
   (import (metrics))
 
   ;;======================================================
   ;;
   ;; Eval:
   ;;
   ;;====================================================== 
   
   (import (r6rs eval))
   
   (eval '(+ 1 2) 
         (environment '(r6rs)))   ;==> 3
   
   (library (foo)
     (export foo-x)
     (import (r6rs))
     (define foo-x 4))
   
   (eval '(+ 1 (let-syntax ((foo (lambda (_) foo-x)))
                 (foo)))
         (environment '(r6rs) '(for (foo) expand)))      ;==> 5
   
   (library (bar) 
     (export)
     (import (r6rs)
             (r6rs eval))
     
     (display
      (eval '(+ 1 (let-syntax ((foo (lambda (_) foo-x)))
                    (foo)))
            (environment '(r6rs) '(for (foo) expand)))))
   
   (import (bar))   ;==> 5
     
   ;;======================================================
   ;;
   ;; General syntax-case expander tests:
   ;;
   ;;======================================================
   
   (import (for (r6rs) run expand (meta 2)))
   
   (let-syntax ((m (lambda (e) 
                     (let-syntax ((n (lambda (e) 3)))
                       (n)))))
     (m))
   
   ;; Some simple patern and template pitfalls:
   
   (syntax-case '((1 2) (3 4)) () 
     (((x ...) ...) (syntax (x ... ...))))     ;==> (1 2 3 4)
   
   ;; R6RS pattern extensions:
   
   (syntax-case '(1 2 3 4) ()
     ((x ... y z) (syntax ((x ...) y z))))     ;==> ((1 2) 3 4)
   
   (syntax-case '(1 2 3 . 4) ()
     ((x ... y . z) (syntax ((x ...) y z))))   ;==> ((1 2) 3 4)
   
   (syntax-case '#(1 2 3 4) ()
     (#(x ... y z)  (syntax (#(x ...) y z))))  ;==> (#(1 2) 3 4)
   
   (syntax-case '((1 2) (3 4)) ()
     (((a b) ...) (syntax ((a ...) (b ...))))) ;==> ((1 3) (2 4))
   
   (syntax-case '((1 2) 3) ()
     (((a b) ...) (syntax ((a ...) (b ...))))
     (_           #f))                         ;==> #f
   
   (syntax-case '((1 2) (3 4) . 3) ()
     (((a b) ... . c) (syntax ((a ...) (b ...))))) ;==> ((1 3) (2 4))
   
   ;; Wildcards:
   
   (let-syntax ((foo (syntax-rules ()
                       ((_ _ _) 'yes))))
     (foo 3 4))
   
   ;; Identifier macros:
   
   (define-syntax foo
     (lambda (e)
       (or (identifier? e)
           (syntax-violation 'foo "Invalid expression" e))
       40))
   
   foo             ;==> 40
   ;; (set! foo 1) ;==> Syntax violation: Syntax being set! is not a variable transformer
   ;; (foo)        ;==> syntax violation: foo - Invalid expression
   
   (import (r6rs mutable-pairs))
   
   (define p (cons 4 5))
   (define-syntax p.car
     (make-variable-transformer
      (lambda (x)
        (syntax-case x (set!)
          ((set! _ e) (syntax (set-car! p e)))
          ((_ . rest) (syntax ((car p) . rest)))
          (_          (syntax (car p)))))))
   (set! p.car 15)
   p.car           ;==> 15
   p               ;==> (15 . 5)
   
   (define p (cons 4 5))
   (define-syntax p.car (identifier-syntax (car p)))
   p.car              ;==> 4
   ;;(set! p.car 15)  ;==> Syntax violation: Syntax being set! is not a variable transformer
   
   (define p (cons 4 5))
   (define-syntax p.car
     (identifier-syntax
      (_          (car p))
      ((set! _ e) (set-car! p e))))
   (set! p.car 15)
   p.car           ;==> 15
   p               ;==> (15 . 5)
   
   ;; Check displaced identifier error:
   
   ;; (let ((x 1))
   ;;   (let-syntax ((foo (lambda (x)
   ;;                       (syntax x))))
   ;;     (foo)))
   ;;             ;==> Syntax error: Attempt to use binding of x at invalid level 0 
   ;;                                Binding is valid at levels (1)
    
   
   ;; Testing toplevel forward references:
   
   (define (f) (g))
   (define (g) 1)
   (f)             ;==> 1
   
   (define-syntax foo (lambda (_) (syntax (bar))))
   (define-syntax bar (lambda (_) 1))
   (foo)           ;==> 1
   
   ;; The following must give an error, since g-0 is bound at level 0 but used at level 1: 
   
   ;; (define-syntax foo (lambda (_) (g-0)))  ;==> Syntax violation: invalid reference
   ;;                                         ;    No binding available for g-0 at meta level 1  
   ;; (define (g-0) 1)
   ;; (foo)             
  
   ;; Correct forward reference (*):
   
   (let ((x 'outer)) 
     (define-syntax foo 
       (syntax-rules ()
         ((_ lhs) (define lhs x))))
     (foo (f))
     (define x 'inner)
     (f))                  ;==> inner
  
   ;; This must give an error:
   ;;
   ;;   (let ()
   ;;     (let-syntax ((foo (lambda (_) (let ((x 2)) (syntax x)))))  
   ;;      (define (f) (foo)))
   ;;     (define x 1)
   ;;     (f))
              ;==>  Attempt to use binding of x at invalid level 0.  Binding is only valid at levels: 1 
   
   ;; Forward references for internal define-syntax works correctly.
   
   (let ()
     (define-syntax odd
       (syntax-rules ()
         ((odd) #t)
         ((odd x . y) (not (even . y)))))
     (define-syntax even
       (syntax-rules ()
         ((even) #f)
         ((even x . y) (not (odd . y)))))
     (odd x x x))                          ;==> #t
   
   ;; Forward reference to procedure from transformer.
   
   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((_) bar)))
     (define bar 1)
     (foo))            ;==> 1
   
   ;; Secrecy of generated toplevel defines:
   
   (define x 1) 
   (let-syntax ((foo (lambda (e)
                       (syntax (begin 
                                 (define x 2)
                                 x)))))
     (foo))  ;==> 2
   x         ;==> 1 
  
   ;; Stress testing expander with internal letrec-generated body,
   ;; begins, etc.
   
   (let ()
     (letrec-syntax ((foo (syntax-rules ()
                            ((_) (begin (define (x) 1)
                                        (begin
                                          (define-syntax y
                                            (syntax-rules ()
                                              ((_) (x))))
                                          (bar y))))))
                     (bar (syntax-rules ()
                            ((_ y) (begin (define (z) (baz (y)))   
                                          (z)))))
                     (baz (syntax-rules ()
                            ((baz z) z))))     
       (foo)))                               ;==> 1
   
   ;; Big stress test, including nested let-syntax and
   ;; forward reference to later define-syntax.
   
   (let ((foo /))
     (letrec-syntax ((foo (syntax-rules ()
                            ((_ z) (begin (define (x) 4)
                                          (define-syntax y
                                            (syntax-rules ()
                                              ((_) (x))))
                                          (bar z y)))))
                     (bar (syntax-rules ()
                            ((_ z y) (define (z) (baz (y))))))  
                     (baz (syntax-rules ()
                            ((baz z) z))))     
       (let-syntax ((foobar (syntax-rules ()   ;; test nested let-syntax
                              ((_ u z)
                               (define-syntax u
                                 (syntax-rules ()
                                   ((_ x y) (z x y))))))))
         (foo a)                
         (foobar gaga goo)))   ;; foobar creates forward reference to goo
                               ;; from expanded transformer.
     (define-syntax goo (syntax-rules ()
                          ((_ x y) (define-syntax x
                                     (syntax-rules ()
                                       ((_) y))))))
     (gaga b (a)) 
     (foo (b)))     ;==> 1/4

   ;; Internal let-syntax, but in a library:
   ;; which is the same algorithm as in a lambda body.  
    
   (library (test)
     (export)
     (import (r6rs))
     (let-syntax ((foo (syntax-rules ()
                         ((_ bar)
                          (begin
                            (define x 7)
                            (define-syntax bar
                              (syntax-rules ()
                                ((_) (display x)))))))))
       (foo baz)
       (baz)))
   
   (import (test))  ;==> 7
   
   (let ((a 1)
         (b 2))
     (+ a b))     ;==> 3
   
   (define-syntax swap!
     (lambda (exp)
       (syntax-case exp ()
         ((_ a b)
          (syntax
           (let ((temp a))
             (set! a b)
             (set! b temp)))))))
   
   (let ((temp 1)
         (set! 2))
     (swap! set! temp)
     (values temp set!))   ;==> 2 1
   
   (let ((x 'outer))
     (let-syntax ((foo (lambda (exp) (syntax x))))
       (let ((x 'inner))
         (foo))))          ;==> outer
   
   ;; SRFI-93 example of expansion of internal definitions
   
   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((foo x) (define x 37))))
     (foo a)
     a)                 ;==> 37
   
   (case 'a
     ((b c) 'no)
     ((d a) 'yes))      ;==> yes
   
   (let ((x 1))
     (let-syntax ((foo (lambda (exp) (syntax x))))
       (let ((x 2))
         (foo))))       ;==> 1
   
   (let ((x 1))
     (let-syntax ((foo (lambda (exp) (datum->syntax (syntax y) 'x))))
       (let ((x 2))
         (foo))))       ;==> 1
   
   (let-syntax ((foo (lambda (exp)
                       (let ((id (cadr exp)))
                         (bound-identifier=? (syntax x)
                                             (syntax id))))))
     (foo x))    ;==> #f
   
   (cond (#f 1) (else 2))                 ;==> 2
   (let ((else #f)) (cond (else 2)))      ;==> unspecified
   
   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x) (syntax
                               (let-syntax ((n (lambda (_)
                                                 (syntax (let ((x 4)) x)))))
                                 (n))))))))
     (m z))   ;==> 4
   
   ;; Expression let-syntax and sequences:
   
   (+ (let-syntax ((foo (lambda (e) 1)))
        (display 'foo)
        (foo))
      2)          ;==> foo 3
   
   (+ (begin (display 'foo)
             1)
      2)          ;==> foo 3
   
   ;;;=========================================================================
   ;;
   ;; Composing macros with intentional variable capture using DATUM->SYNTAX
   ;;
   ;;;=========================================================================
   
   (define-syntax if-it
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2 e3)
          (with-syntax ((it (datum->syntax (syntax k) 'it)))
            (syntax (let ((it e1))
                      (if it e2 e3))))))))
   
   (define-syntax when-it
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2)
          (with-syntax ((it* (datum->syntax (syntax k) 'it)))
            (syntax (if-it e1
                           (let ((it* it)) e2)
                           (if #f #f))))))))
   
   (define-syntax my-or
     (lambda (x)
       (syntax-case x ()
         ((k e1 e2)
          (syntax (if-it e1 it e2))))))
   
   (if-it 2 it 3)    ;==> 2
   (when-it 42 it)   ;==> 42
   (my-or 2 3)       ;==> 2
   ;;(my-or #f it)   ;==> undefined identifier: it
   
   (let ((it 1)) (if-it 42 it #f))   ;==> 42
   (let ((it 1)) (when-it 42 it))    ;==> 42
   (let ((it 1)) (my-or #f it))      ;==> 1
   (let ((if-it 1)) (when-it 42 it)) ;==> 42
   
   ;;;=========================================================================
   ;;
   ;; Escaping ellipses:
   ;;
   ;;;=========================================================================
   
   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x ...)
                        (with-syntax ((::: (datum->syntax (syntax here) '...)))
                          (syntax
                           (let-syntax ((n (lambda (form)
                                             (syntax-case form ()
                                               ((_ x ... :::)
                                                (syntax `(x ... :::)))))))
                             (n a b c d)))))))))
     (m u v))
   
   ;;==> (a b c d)
   
   (let-syntax ((m (lambda (form)
                     (syntax-case form ()
                       ((_ x ...)
                        (syntax
                         (let-syntax ((n (lambda (form)
                                           (syntax-case form ()
                                             ((_ x ... (... ...))
                                              (syntax `(x ... (... ...))))))))
                           (n a b c d))))))))
     (m u v))
   
   ;;==> (a b c d)
   
   ;;;=========================================================================
   ;;
   ;; From R5RS:
   ;;
   ;;;=========================================================================
   
   (define-syntax or
     (syntax-rules ()
       ((or)          #f)
       ((or e)        e)
       ((or e1 e ...) (let ((temp e1))
                        (if temp temp (or e ...))))))
   
   (or #f #f 1)  ;==> 1
   
   (define-syntax or
     (lambda (form)
       (syntax-case form ()
         ((or)          (syntax #f))
         ((or e)        (syntax e))
         ((or e1 e ...) (syntax (let ((temp e1))
                                  (if temp temp (or e ...))))))))
   
   (or #f #f 1)  ;==> 1
   
   (let-syntax ((when (syntax-rules ()
                        ((when test stmt1 stmt2 ...)
                         (if test
                             (begin stmt1
                                    stmt2 ...))))))
     (let ((if #t))
       (when if (set! if 'now))
       if))                                  ;===>  now
   
   (let ((x 'outer))
     (let-syntax ((m (syntax-rules () ((m) x))))
       (let ((x 'inner))
         (m))))                              ;===>  outer
   
   (letrec-syntax
       ((my-or (syntax-rules ()
                 ((my-or) #f)
                 ((my-or e) e)
                 ((my-or e1 e2 ...)
                  (let ((temp e1))
                    (if temp
                        temp
                        (my-or e2 ...)))))))
     (let ((x #f)
           (y 7)
           (temp 8)
           (let odd?)
           (if even?))
       (my-or x
              (let temp)
              (if y)
              y)))                ;===>  7
   
   (define-syntax cond
     (syntax-rules (else =>)
       ((cond (else result1 result2 ...))
        (begin result1 result2 ...))
       ((cond (test => result))
        (let ((temp test))
          (if temp (result temp))))
       ((cond (test => result) clause1 clause2 ...)
        (let ((temp test))
          (if temp
              (result temp)
              (cond clause1 clause2 ...))))
       ((cond (test)) test)
       ((cond (test) clause1 clause2 ...)
        (let ((temp test))
          (if temp
              temp
              (cond clause1 clause2 ...))))
       ((cond (test result1 result2 ...))
        (if test (begin result1 result2 ...)))
       ((cond (test result1 result2 ...)
              clause1 clause2 ...)
        (if test
            (begin result1 result2 ...)
            (cond clause1 clause2 ...)))))
   
   (let ((=> #f))
     (cond (#t => 'ok)))                   ;===> ok
   
   (cond ('(1 2) => cdr))                  ;===> (2)
   
   (cond ((> 3 2) 'greater)
         ((< 3 2) 'less))                 ;===>  greater
   (cond ((> 3 3) 'greater)
         ((< 3 3) 'less)
         (else 'equal))                   ;===>  equal
   
   ;; Eli Barzilay
   ;; In thread:
   ;; R5RS macros...
   ;; http://groups.google.com/groups?selm=skitsdqjq3.fsf%40tulare.cs.cornell.edu
   
   (let-syntax ((foo
                 (syntax-rules ()
                   ((_ expr) (+ expr 1)))))
     (let ((+ *))
       (foo 3)))               ;==> 4
   
   ;; Al Petrofsky again
   ;; In thread:
   ;; Buggy use of begin in core:primitives cond and case macros.
   ;; http://groups.google.com/groups?selm=87bse3bznr.fsf%40radish.petrofsky.org
   
   (let-syntax ((foo (syntax-rules ()
                       ((_ var) (define var 1)))))
     (let ((x 2))
       (begin (define foo +))
       (cond (else (foo x)))
       x))                    ;==> 2
   
   ;; Al Petrofsky
   ;; In thread:
   ;; An Advanced syntax-rules Primer for the Mildly Insane
   ;; http://groups.google.com/groups?selm=87it8db0um.fsf@radish.petrofsky.org
   
   (let ((x 1))
     (let-syntax
         ((foo (syntax-rules ()
                 ((_ y) (let-syntax
                            ((bar (syntax-rules ()
                                    ((_) (let ((x 2)) y)))))
                          (bar))))))
       (foo x)))                        ;==> 1
   
   ;; another example:
   
   (let ((x 1))
     (let-syntax
         ((foo (syntax-rules ()
                 ((_ y) (let-syntax
                            ((bar (syntax-rules ()
                                    ((_ x) y))))
                          (bar 2))))))
       (foo x)))                         ;==> 1
   
   ;; Al Petrofsky
   
   (let ((a 1))
     (letrec-syntax
         ((foo (syntax-rules ()
                 ((_ b)
                  (bar a b))))
          (bar (syntax-rules ()
                 ((_ c d)
                  (cons c (let ((c 3))
                            (list d c 'c)))))))
       (let ((a 2))
         (foo a))))                ;==> (1 2 3 a)
   
   (let ((=> #f))
     (cond (#t => 'ok)))                   ;===> ok
   
   (cond ('(1 2) => cdr))                  ;===> (2)
   
   (cond ((< 3 2) 'less)
         ((> 3 2) 'greater))               ;===>  greater
   
   (cond ((> 3 3) 'greater)
         ((< 3 3) 'less)
         (else 'equal))                    ;===>  equal
   
   (define-syntax loop     ;; no change
     (lambda (x)
       (syntax-case x ()
         ((k e ...)
          (with-syntax ((break (datum->syntax (syntax k) 'break)))
            (syntax (call-with-current-continuation
                     (lambda (break)
                       (let f () e ... (f))))))))))
   
   (let ((n 3) (ls '()))
     (loop
      (if (= n 0) (break ls))
      (set! ls (cons 'a ls))
      (set! n (- n 1))))    ;==> (a a a)
   
   (let ((x '(1 3 5 7 9)))
     (do ((x x (cdr x))
          (sum 0 (+ sum (car x))))
       ((null? x) sum)))                ;==>  25
   
   (define-syntax define-structure    
     (lambda (x)
       (define gen-id
         (lambda (template-id . args)
           (datum->syntax template-id
                          (string->symbol
                           (apply string-append
                                  (map (lambda (x)
                                         (if (string? x)
                                             x
                                             (symbol->string
                                              (syntax->datum x))))
                                       args))))))
       (syntax-case x ()
         ((_ name field ...)
          (with-syntax
              ((constructor (gen-id (syntax name) "make-" (syntax name)))
               (predicate (gen-id (syntax name) (syntax name) "?"))
               ((access ...)
                (map (lambda (x) (gen-id x (syntax name) "-" x))
                     (syntax (field ...))))
               ((assign ...)
                (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                     (syntax (field ...))))
               (structure-length (+ (length (syntax (field ...))) 1))
               ((index ...) (let f ((i 1) (ids (syntax (field ...))))
                              (if (null? ids)
                                  '()
                                  (cons i (f (+ i 1) (cdr ids)))))))
            (syntax (begin
                      (define constructor
                        (lambda (field ...)
                          (vector 'name field ...)))
                      (define predicate
                        (lambda (x)
                          (and (vector? x)
                               (= (vector-length x) structure-length)
                               (eq? (vector-ref x 0) 'name))))
                      (define access (lambda (x) (vector-ref x index))) ...
                      (define assign
                        (lambda (x update)
                          (vector-set! x index update)))
                      ...)))))))
   
   (define-structure tree left right)
   (define t
     (make-tree
      (make-tree 0 1)
      (make-tree 2 3)))
   
   t                     ;==> #(tree #(tree 0 1) #(tree 2 3))
   (tree? t)             ;==> #t
   (tree-left t)         ;==>#(tree 0 1)
   (tree-right t)        ;==> #(tree 2 3)
   (set-tree-left! t 0)
   t                     ;==> #(tree 0 #(tree 2 3))
   
   ;; Quasisyntax tests:
   
   (define-syntax swap!
     (lambda (e)
       (syntax-case e ()
         ((_ a b)
          (let ((a (syntax a))
                (b (syntax b)))
            (quasisyntax
             (let ((temp #,a))
               (set! #,a #,b)
               (set! #,b temp))))))))
   
   (let ((temp 1)
         (set! 2))
     (swap! set! temp)
     (values temp set!))   ;==> 2 1
   
   (define-syntax case
     (lambda (x)
       (syntax-case x ()
         ((_ e c1 c2 ...)
          (quasisyntax
           (let ((t e))
             #,(let f ((c1    (syntax c1))
                       (cmore (syntax (c2 ...))))
                (if (null? cmore)
                    (syntax-case c1 (else)
                      ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                      (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                       (begin e1 e2 ...)))))
                    (syntax-case c1 ()
                      (((k ...) e1 e2 ...)
                       (quasisyntax
                        (if (memv t '(k ...))
                            (begin e1 e2 ...)
                            #,(f (car cmore) (cdr cmore))))))))))))))
   
   (case 'a
     ((b c) 'no)
     ((d a) 'yes))
   
   (define-syntax let-in-order
     (lambda (form)
       (syntax-case form ()
         ((_ ((i e) ...) e0 e1 ...)
          (let f ((ies (syntax ((i e) ...)))
                  (its (syntax ())))
            (syntax-case ies ()
              (()            (quasisyntax (let #,its e0 e1 ...)))
              (((i e) . ies) (with-syntax (((t) (generate-temporaries '(t))))
                               (quasisyntax
                                (let ((t e))
                                  #,(f (syntax ies)
                                      (quasisyntax
                                       ((i t) #,@its)))))))))))))
   
   (let-in-order ((x 1)
                  (y 2))
                 (+ x y))                ;==> 3
   
   (let-syntax ((test-ellipses-over-unsyntax
                 (lambda (e)
                   (let ((a (syntax a)))
                     (with-syntax (((b ...) '(1 2 3)))
                       (quasisyntax
                        (quote ((b #,a) ...))))))))
     (test-ellipses-over-unsyntax))
   
   ;==> ((1 a) (2 a) (3 a))
   
   ;; Some tests found online (Guile?)
   
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax
                    '(list #,(+ 1 2) 4)))))
     (test))
   ;==> (list 3 4)
   
   (let-syntax ((test
                 (lambda (_)
                   (let ((name (syntax a)))
                     (quasisyntax '(list #,name '#,name))))))
     (test))
   ;==> (list a 'a)
   
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax '(a #,(+ 1 2) #,@(map abs '(4 -5 6)) b)))))
     (test))
   ;==> (a 3 4 5 6 b)
   
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax '((foo #,(- 10 3)) #,@(cdr '(5)) . #,(car '(7)))))))
     (test))
   ;==> ((foo 7) . 7)
   
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax #,(+ 2 3)))))
     (test))
   ;==> 5
   
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax
                    '(a (quasisyntax (b #,(+ 1 2) #,(foo #,(+ 1 3) d) e)) f)))))
     (test))
   ;==> (a (quasisyntax (b #,(+ 1 2) #,(foo 4 d) e)) f)
   
   (let-syntax ((test
                 (lambda (_)
                   (let ((name1 (syntax x)) (name2 (syntax y)))
                     (quasisyntax
                      '(a (quasisyntax (b #,#,name1 #,(syntax #,name2) d)) e))))))
     (test))
   ;==> (a (quasisyntax (b #,x #,(syntax y) d)) e)
   
   ;; Bawden's extensions:
   
   (let-syntax ((test
                 (lambda (_) 
                   (quasisyntax '(a (unsyntax 1 2) b)))))
     (test))
   ;==> (a 1 2 b)
    
   (let-syntax ((test
                 (lambda (_)
                   (quasisyntax '(a (unsyntax-splicing '(1 2) '(3 4)) b)))))
     (test))
   ;==> (a 1 2 3 4 b) 
   
   (let-syntax ((test
                 (lambda (_)
                   (let ((x (syntax (a b c))))
                     (quasisyntax '(quasisyntax (#,#,x #,@#,x #,#,@x #,@#,@x)))))))
     (test))
   
   ;;==> (quasisyntax (#,(a b c) #,@(a b c) (unsyntax a b c) (unsyntax-splicing a b c)))
   ;;     which is equivalent to
   ;;    (quasisyntax (#,(a b c) #,@(a b c) #,a #,b #,c #,@a #,@b #,@c)
   ;;     in the Bawden prescripion
   
   )) ;; repl

