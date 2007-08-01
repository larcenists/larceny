
;;;=====================================================================
;;;
;;; Examples and tests:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html   
;;; 
;;;=====================================================================

(load "expander.scm")
;; Compile standard libraries.
;; This only needs to be done once as long as (ex:unique-token)
;; in compat-*.scm gives a globally unique token.
($ex:expand-file "standard-libraries.scm" "standard-libraries.exp")
(load "standard-libraries.exp") 
  
;;======================================================================
;;
;; Example of compiling a set of libraries
;; and a program all in the same file:
;;
;;======================================================================

;; Compiling.

($ex:expand-file "sample-libs-and-program.scm" "sample-libs-and-program.exp")

;; Executing.

(load "sample-libs-and-program.exp")  ;==> displays 3

;;======================================================================
;;
;; Example of compiling and executing a set of libraries
;; and programs all in different files, possibly in
;; different sessions:
;;
;;======================================================================

;; Expand-file requires dependencies to be listed.
;; For example, (party) imports (stack) and (balloons),
;; so we require compiled versions of the latter while
;; compiling the former.  

($ex:expand-file "sample-stack.scm" "sample-stack.exp")
($ex:expand-file "sample-balloons.scm" "sample-balloons.exp")
($ex:expand-file "sample-party.scm" "sample-party.exp" "sample-stack.exp" "sample-balloons.exp")
($ex:expand-file "sample-program.scm" "sample-program.exp" "sample-party.exp")

;; Executing program.
;; Required libraries must be loaded first.
;; Note that loading a compiled library neither visits
;; nor invokes it - it just makes the library
;; available to be visited or invoked by clients.  

(load "sample-stack.exp")
(load "sample-balloons.exp")
(load "sample-party.exp")
(load "sample-program.exp")  ;==> displays Boom! 108 Boom! 24

;;======================================================================
;;
;; Example REPL session.
;; In practice, replace the built-in REPL evaluator
;; with $ex:repl.
;;
;;======================================================================

($ex:repl
 '( 
   ;; Make rnrs available at toplevel:
   
   (import (rnrs))
   
   ;; Reflection facility useful for development and debugging:
   ;; See uses of (environment-bindings ---) in examples below:

   (import (only (rnrs eval)            environment)
           (only (rnrs eval reflection) environment-bindings))

   ;;;=====================================================================
   ;;;
   ;;; LIBRARIES AND PROGRAMS:
   ;;;
   ;;; The file standard-libraries.scm builds r6rs up using a sequence 
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
     (import (rnrs)
             (rnrs mutable-pairs))
     
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
     (import (rnrs))
     
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
     (import (rnrs)
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
     (import (rnrs) (party))
     (define p (make-party))
     (pop! p)                      ;; displays "Boom! 108"
     (push! p (push (make 5 5) 1))
     (pop! p))                     ;; displays "Boom! 24"
   
   ;; This completes the program, executing main:
   
   (program
    (import (main)))
   
   ;; In the current implementation, one can also do
   ;; this to execute a library in the REPL:
   
   (import (main))

   ;;======================================================================
   ;;
   ;; Library reflection:
   ;;
   ;;======================================================================
   
   (environment-bindings (environment '(party)))
   
   ;;  ==> (((name make)       (type variable) (from (balloons)) (levels (0)))
   ;;       ((name push)       (type variable) (from (balloons)) (levels (0)))
   ;;       ((name push!)      (type variable) (from (stack))    (levels (0)))
   ;;       ((name make-party) (type variable) (from (party))    (levels (0)))
   ;;       ((name pop!)       (type variable) (from (party))    (levels (0))))
   ;;======================================================================
   ;;
   ;; Macros and meta-levels
   ;;
   ;;======================================================================
   
   (library (my-helpers id-stuff)
     (export find-dup)
     (import (rnrs))
     
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
     (import (rnrs)
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

   ;; Do some reflection to see what is going on with the levels:

   (environment-bindings (environment '(for (my-helpers id-stuff) expand)))

   ;;==> (((name   find-dup)
   ;;      (type   variable)
   ;;      (from   (my-helpers id-stuff))
   ;;      (levels (1))))
   
   (library (let-div)
     (export let-div)
     (import (rnrs) (my-helpers value-stuff))
     
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
    (import (let-div) (rnrs))   
    (let-div 5 2 (q r) (+ q r))  ;==> 3
    )
   
   ;;=================================================================
   ;;
   ;; Version reference syntax:
   ;;
   ;;=================================================================
   
   (library (foo (2 3 5))
     (export)
     (import))
   
   (import (foo ()))
   (import (foo (2)))
   (import (foo (2 3)))
   (import (foo (or (1 (>= 1)) (2))))
   (import (foo ((or 1 2 3))))

   ;;======================================================
   ;;
   ;; Further program tests:
   ;;
   ;;======================================================
     
   ;; Test expressions returning no values and more than one value
   ;; preceding definitions:
   
   (program
    (import (rnrs))
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
     (import (rnrs))
     (define u 1))
   
   (library (bar)
     (export u v)
     (import (rnrs) (foo))
     (define-syntax v (lambda (e) (syntax u))))
   
   (library (baz)
     (export)
     (import (for (rnrs) (meta 2) expand run) 
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
     (import (rnrs))
     (define x 2)
     (define y 4))
   
   (library (baz)
     (export y)                      ;; exports y at level 1
     (import (rnrs) (for (foo) expand)))

   ;; To see what is going on, do some introspection:
   
   (environment-bindings (environment '(baz)))
   
        ;; ==> (((name y) (type variable) (from (foo)) (levels (1))))
   
   (library (bab)
     (export f)
     (import (for (rnrs) expand run)   ;; This also implicitly imports into (meta 2) 
             (for (foo)  expand)       ;; imports x and y at level 1
             (for (baz)  expand))      ;; also imports y but at level expand + 1 = 2
     (define (f) 
       (let-syntax ((foo (lambda (_) 
                           (+ x                                   ;; level 1
                              y                                   ;; level 1 
                              (let-syntax ((bar (lambda (_) y)))  ;; level 2
                                (bar))))))
         (foo))))

   ;; Again, do some reflection to see what is going on with the levels:

   (environment-bindings (environment '(for (foo)  expand) 
                                      '(for (baz)  expand)))
   
   ;;==> (((name x) (type variable) (from (foo)) (levels (1)))
   ;;     ((name y) (type variable) (from (foo)) (levels (2 1))))
   
   (import (bab))
   (f)   ;==> 10



   
   ;;==========================================================
   ;;
   ;; Check that levels of reference are determined lexically:
   ;;
   ;;==========================================================
   
   (library (foo)  
     (export f) 
     (import (rnrs))
     (define (f) 1)) 
 
   (library (bar) 
     (export g) 
     (import (rnrs) 
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
   ;;   (import (rnrs) (foo) (bar))   
   ;;   (display (g)))         
   
   ;; ==> Syntax violation: Attempt to use binding of f in library (bar) at invalid meta level 0. 
   ;;     Binding is only valid at meta levels: 1 
   
   ;; Example from http://www.r6rs.org/r6rs-editors/2006-August/001682.html
   
   (library (A)
     (export x)
     (import (rnrs))
     (define x 37))
   
   (library (B)
     (export)
     (import (A)))
   
   (library (C)
     (export foo)
     (import (rnrs) (for (A) expand))
     (define-syntax foo
       (syntax-rules ()
         ((_) x))))
   
   (library (D)
     (export foo)
     (import (rnrs) (C)))
   
   ;; This has to raise syntax error to avoid problem described in 
   ;; above message.
   
   (library (E)
     (export)
     (import (rnrs) (B) (D))
     ;; (display (foo))  ; Attempt to use x in library (C) at invalid meta level 0.  
     ;;                  ; Binding is only available at meta levels: 1 
     )
  
   ;;==============================================================
   ;;
   ;; Importing into multiple and negative levels: 
   ;;
   ;;==============================================================
   
   (library (foo) 
     (export x)
     (import (rnrs))
     (define x 42))
   
   (library (bar)
     (export get-x)
     (import (rnrs)
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
     (import (for (rnrs) (meta 3) (meta 2) expand run) 
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
     
     ;; This should give a syntax error:
     
     ;; (display 
     ;;  (let-syntax ((m (lambda (ignore)
     ;;                     (let-syntax ((n (lambda (ignore)
     ;;                                       (get-x))))
     ;;                       (n)))))
     ;;   (m)))              ;==> Syntax-violation: Attempt to use binding of get-x in library (baz) at invalid level 2 
     ;;                      ;                      Binding is only valid at levels (1 3) 
     
     ) ;; baz
   
   (import (baz))   ;==> 42 42
   
   ;;=============================================================
   ;;
   ;; Nice practical example of negative levels:
   ;; Modified from example by Abdulaziz Ghuloum.
   ;;
   ;;=============================================================
      
   (library (print)
     (export print-args)
     (import (rnrs))
     (define print-args
       (lambda (fml* act*)
         (display "Lambda ")
         (display fml*)
         (display " : ")
         (display act*)
         (newline))))
   
   (library (tracers-helpers)
     (export trace-transformer untrace-transformer)
     (import (for (rnrs)  (meta -1) run) 
             (for (print) (meta -1)))
     (define trace-transformer
       (lambda (stx)
         (syntax-case stx ()
           ((_ fml* b b* ...)
            (syntax
             (lambda act*
               (print-args 'fml* act*)
               (apply (lambda fml* b b* ...) act*)))))))
     (define untrace-transformer
       (lambda (stx)
         (syntax-case stx ()
           ((_ fml* b b* ...)
            (syntax
             (lambda fml* b b* ...)))))))
     
   (library (tracers)
     (export trace-region untrace-region)
     (import (rnrs)
             (for (tracers-helpers) expand))
   
     (define-syntax trace-region
       (lambda (x)
         (syntax-case x ()
           ((kwd b b* ...)
            (with-syntax ((L (datum->syntax (syntax kwd) 'lambda)))
              (syntax
               (let-syntax ((L trace-transformer))
                 b b* ...)))))))
     
     (define-syntax untrace-region
       (lambda (x)
         (syntax-case x ()
           ((kwd b b* ...)
            (with-syntax ((L (datum->syntax (syntax kwd) 'lambda)))
              (syntax
               (let-syntax ((L untrace-transformer))
                 b b* ...))))))))
   
   (library (FOO)
     (export)
     (import (rnrs) (tracers))
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
     (import (rnrs))
     (define counter
       (let ((x 0)) 
         (lambda () 
           (set! x (+ x 1))
           x))))
   
   (library (bar)
     (export)
     (import (rnrs) 
             (for (foo) run expand))
     (let-syntax ((m (lambda (e) (counter))))
       (display (list (m) (counter)))))
  
   (import (bar))    ;==> (1 1) since bindings are never shared between expand and run
  
   (library (baz)
     (export)
     (import (for (rnrs) run expand)
             (for (foo) expand (meta 2)))
     (let-syntax ((_ (let-syntax ((m (lambda (e) (counter))))
                       (display (list (m) (counter)))
                       (lambda (_) _))))))
   
   ;;                             ;==>  (1 1) when bindings are not shared at expand-time
   ;;                                   (1 2) when bindings are shared at expand-time
  
   ;; This detects whether macro bindings are shared between levels.
   
   (library (foo)
     (export f)
     (import (rnrs))
     (define f
       (let ((x 0)) 
         (lambda () 
           (set! x (+ x 1))
           x))))
   
   (library (bar)
     (export m)
     (import (rnrs) (for (foo) expand))
     (define-syntax m
       (lambda (e)
         (f))))
   
   (library (baz)
     (export)
     (import (for (rnrs) run expand) 
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
     (import (rnrs))
     (define x 1))
   
   (library (foo)
     (export template)
     (import (for (baz) expand)
             (for (rnrs) run (meta -1)))
     (define (template)
       (syntax (let-syntax ((m (lambda (_) 
                                 (let-syntax ((n (lambda (_) x)))  ; <== (1)
                                   (n)))))
                 (m))))
     (let-syntax ((n (lambda (_) x)))  ; <== (2)
       (n)))
   
   (library (bar)
     (export)
     (import (rnrs)
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
     (import (rnrs))
     (define v #f)
     (define (put! x) (set! v x))
     (define (get) v))
   
   (library (B)
     (export b)
     (import (rnrs))
     (define b 7))
   
   (library (A)
     (export do-a)
     (import (rnrs)
             (for (D) expand)
             (for (B) run))  
     
     (define-syntax do-a
       (begin (put! (syntax b))
              (lambda (stx)
                #f))))
   
   (library (C)
     (export)
     (import (for (rnrs) run expand)
             (for (D) expand)
             
             ;; In this test, change RUN to EXPAND.  The following syntax
             ;; error should be obtained:
             ;;
             ;; Syntax violation: invalid reference
             ;; Attempt to use b in library (A) at invalid meta level -1.  
             ;; Binding is only available at meta levels: 0 
             
             (for (A) run)) 
     
     (define-syntax make-ref
       (lambda (stx)
         (get)))
   
     (display (make-ref)))
   
   (import (C))
   
   ;;=================================================================
   ;;
   ;; Phase checking for literals and free-identifier=? uses:
   ;;
   ;; R6RS does not say if these operations count as references,
   ;; but does require importing of literals such as ... and _
   ;; into the appropriate phase.  To ensure maximal portability,
   ;; a good implementation should have a liberal interpretation of
   ;; what a reference is, and check literal phases.
   ;; We do so by checking that the arguments of free-identifier=?
   ;; are both in phase when the result of the comparison is #t
   ;; (for examples why, see below).
   ;; For ensuring that uses of literals are in phase, this is sufficient
   ;; but may be more than necessary.  However, we are within our R6RS
   ;; rights to be more restrictive, and in this case more is
   ;; better for maximal portability checking.  
   ;;
   ;;=================================================================

   ;; This shows why we should only test that the arguments of free-identifier=?
   ;; are in phase when the comparison succeeds.  The expander should complain
   ;; about the wrong phase of ..., but should not complain about the phase of
   ;; LIST in the template, even though LIST is also compared to see if it
   ;; is bound to R6RS ... during expansion.
   
   ;;(library (foo)
   ;;  (export)
   ;;  (import (except (rnrs base) ...)
   ;;          (only (rnrs syntax-case) ...))  ;; .. is in wrong phase
   ;;  (define-syntax list-macro
   ;;    (syntax-rules ()
   ;;      ((_ x ...) (list x ...)))))
   ;;
   ;;   Attempt to use binding ... in library foo at invalid meta level 1.
   ;;   Binding is only available at meta levels: 0 

   ;; More examples:

   (library (foo)
     (export test)
     (import (rnrs base))
     (define-syntax test
       (syntax-rules (car)
         ((_ car) #t)
         ((_ k)   #f))))

   ;; The literal CAR is used in the wrong phase:
   ;;
   ;;(library (bar)
   ;;   (export)
   ;; (import (for (rnrs base) (meta 21))
   ;;          (foo))
   ;;  (test car))
   ;;   
   ;;    Attempt to use binding car in library bar at invalid meta level 0.
   ;;    Binding is only available at meta levels: 21 

   ;; This is not a literal use, so no problem.
   
   (library (bar)
     (export)
     (import (for (rnrs base) (meta 21))
             (foo))
     (test cdr))

   ;; Here the literal is in the wrong phase at the definition site:
   
   (library (foo)
     (export test)
     (import (except (rnrs base) car)
             (for (only (rnrs base) car) (meta 21)))
     (define-syntax test
       (syntax-rules (car)
         ((_ car) #t)        ; is this a reference?
         ((_ k)   #f))))

   ;; The problem is detected as soon as we try to use it:
   
   ;;(library (bar)
   ;;  (export)
   ;;  (import (rnrs base)
   ;;          (foo))
   ;;  (test car))
   ;;
   ;;    Attempt to use binding car in library foo at invalid meta level 0.
   ;;    Binding is only available at meta levels: 21 

   ;; We will not detect the problem if we do not use the literal, though.
   
   (library (bar)
     (export)
     (import (rnrs base)
             (foo))
     (test cdr))     ;==> expands without problems

   ;; To summarize, the arguments of free-identifier=? are only
   ;; required to be in phase when the comparison succeeds.
   
   (library (foo)
     (export test1 test2)
     (import (except (rnrs) car)
             (for (only (rnrs) car) (meta 21)))
     (define-syntax test1
       (lambda (form)
         (free-identifier=? (syntax car) (syntax cdr))))
     (define-syntax test2
       (lambda (form)
         (free-identifier=? (syntax car) (syntax car)))))
   
   (import (foo))
   (test1)  ;==> #f
                                        
   ;;(test2)
   ;;
   ;;    Attempt to use binding car in library foo at invalid meta level 0.
   ;;    Binding is only available at meta levels: 21 22 

   ;; Another example, similar in spirit to the first ... example.

   ;;(library (foo)
   ;;  (export)
   ;;  (import (rnrs base)
   ;;          (for (prefix (only (rnrs base) quasiquote) meta-) (meta 21)))
   ;;  `(meta-quasiquote 1))
   ;;
   ;;   Attempt to use binding meta-quasiquote in library foo at invalid meta level 0.
   ;;   Binding is only available at meta levels: 21 

   
   ;; Out of phase uses of the literals DEFINE, BEGIN, etc., in
   ;; bodies are detected in the same way.  
   
   ;;(library (foo)
   ;;  (export)
   ;;  (import (except (rnrs base) define)
   ;;          (for (only (rnrs base) define)))  ; imported but for no levels
   ;;  (define x 1)                              
   ;;  (display x))
   ;;
   ;;    Attempt to use binding define in library foo at invalid meta level 0.
   ;;    Binding is only available at meta levels: 


   ;;==================================================================
   ;;    
   ;; Out of context reference to let-syntax binding should raise
   ;; syntax error for portability. 
   ;;
   ;;==================================================================   
   
   ;;   (library (foo)
   ;;     (export bar)
   ;;     (import (rnrs))
   ;;     (let-syntax ((baz (lambda (form) 1)))
   ;;       (define-syntax bar
   ;;         (syntax-rules ()
   ;;           ((_) (baz))))))
   ;;   
   ;;   (import (foo))
   ;;   (bar)   ;==> Syntax violation: Reference to macro keyword out of context: baz

   ;;================================================================
   ;;
   ;; Toplevel interaction with unbound identifiers and literals:
   ;; This is unspecified in r6rs but seems like reasonable behaviour.
   ;;
   ;;================================================================

   ;; Toplevel binding should not capture unbound identifiers in
   ;; libraries.

      (library (foo)
        (export bar)
        (import (rnrs))
        (define-syntax bar (syntax-rules () ((_) (baz)))))
      
      (import (foo))
      (define (baz) 1)
    ;;  (define (f) (bar)) ;==> Syntax violation: invalid reference.
    ;;                     ;    No binding available for baz at meta level 0

   ;; Unbound literals in library match unbound identifiers at toplevel.
   
   (library (foo)
     (export bar)
     (import (rnrs))
     (define-syntax bar
       (syntax-rules (unbound-literal)
         ((_ unbound-literal) #t)
         ((_ _)               #f))))

   (import (foo))

   (bar unbound-literal)      ;==> #t
   (bar x)                    ;==> #f
   (define unbound-literal 1)
   (bar unbound-literal)      ;==> #f
   
   ;;======================================================
   ;;
   ;; Example from Matthew Flatt's paper.
   ;; Illustrates use of expand-time state.
   ;;
   ;;====================================================== 
    
   (library (records-helper)
     (export register! registered?)
     (import (rnrs))
     (define table '())
     (define (register! name)
       (set! table (cons name table)))
     (define (registered? name)
       (memp (lambda (entry) (free-identifier=? name entry))
             table)))
     
   (library (records) 
     (export define-record record-switch)
     (import (rnrs) (for (records-helper) expand))
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
     (import (rnrs) (zoo) (records))
     (display 
      (record-switch 'zebra (zebra 'zebra))))   ;==> zebra
   
   (import (metrics))
   
   ;;=================================================================
   ;;
   ;; Immutabilty tests:
   ;;
   ;;=================================================================
   
   (library (foo)
     (export x bar baz bax
             ;; Uncomment to test error on attempt to export mutable variable:        
             ;; z      ; Syntax violation: Attempt to export mutable variable
             )
     (import (rnrs))
     (define x 1)
     (define y 1)
     (define z 1)
     (define-syntax bar
       (syntax-rules ()
         ((_) (set! x 2))))
     (define-syntax baz
       (syntax-rules ()
         ((_) (set! y 2))))
     (define-syntax bax
       (syntax-rules ()
         ((_) z)))
     (set! z 2))
     
   (library (boo)
     (export)
     (import (rnrs) 
             (foo))
     
     ;; Uncomment to test explicit import assigment errors:
     
     ;;(set! x 2))    ; Syntax violation: Imported variable cannot be assigned: x
     ;;(bar)          ; Syntax violation: Imported variable cannot be assigned: x
     
     ;; Uncomment to test implicit import assignment error:
     
     ;;(baz)          ; Syntax violation: Imported variable cannot be assigned: y 
     
     ;; Uncomment to test error on attempt to implicitly import mutable variable:
     
     ;;(bax)          ; Syntax violation: Attempt to implicitly import variable 
     ;;               ;                   that is mutable in library (foo) : z

     ) ; (boo)

   ;;====================================================================
   ;;
   ;; Correct lexical scoping of expansion algorithm: 
   ;;
   ;; Examples where and error should be thrown to avoid giving a 
   ;; lexical-scope-violating semantics to expressions: 
   ;;
   ;;====================================================================
   
   ;; This must give an error:

   ;; (let ()
   ;;   (define-syntax foo (lambda (e) (+ 1 2)))
   ;;   (define + 2)   
   ;;   (foo))      ; Syntax violation: Definition of identifier that may have
   ;;               ; already affected meaning of undeferred portions of body: +
   
   ;; This gives no error:

   (let ()
     (define-syntax foo (lambda (e) (let ((+ -)) (+ 1 2))))
     (define + 2)
     (foo))         ;==> -1
   
   ;;(let ((x #f))
   ;;  (let-syntax ((foo (syntax-rules (x)
   ;;                      ((_ x y) (define y 'outer))
   ;;                      ((_ _ y) (define y 'inner)))))
   ;;    (let ()
   ;;      (foo x p)
   ;;      (define x #f)
   ;;      p)))          ; Syntax violation: Definition of identifier that may have
   ;;                    ; already affected meaning of undeferred portions of body: x

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
   ;;      p)))          ; Syntax violation: Definition of identifier that may have
   ;;                    ; already affected meaning of undeferred portions of body: x

   ;;(let-syntax ((def0 (syntax-rules ()
   ;;                     ((_ x) (define x 0)))))
   ;;  (let ()
   ;;    (def0 z)
   ;;    (define def0 '(def 0))
   ;;    (list z def0))) ; Syntax violation: Definition of identifier that may have
   ;;                    ; already affected meaning of undeferred portions of body: def0

   ;;(let ()
   ;;  (define define 17)
   ;;  define)        ; Syntax violation: Definition of identifier that may have
   ;;                 ; already affected meaning of undeferred portions of body: define

   ;; (define-syntax foo (syntax-rules () ((_ x) (define x 1))))
   ;;   (let ((b 2))
   ;;     (foo a)
   ;;     (define (foo x) 2)
   ;;     (foo b)
   ;;     (values a b)) ; Syntax violation: Definition of identifier that may have
   ;;                   ; already affected meaning of undeferred portions of body: foo

   ;; (define-syntax foo (syntax-rules () ((_ x) (define x 1))))
   ;;   (let ()
   ;;     (foo a)
   ;;     (define-syntax foo (syntax-rules () ((_ x) (define x 2))))
   ;;     (foo b)
   ;;     (values a b))  ; Syntax violation: Definition of identifier that may have
   ;;                    ; already affected meaning of undeferred portions of body: foo

   ;; This should still be valid.
   
   (let ()
     (define-syntax foo
       (syntax-rules ()
         ((_ def0) (def0 define 17))))
     (foo define)
     0)

   ;; Distinguishing literals from non-literal data:
   
   (let ()
     (define-syntax list-macro
       (syntax-rules ()
         ((_ x ...) (list x ...))))
     ;; This must give violation:
     ;;(define ... 1)  ; Syntax violation: Definition of identifier that may have already
     ;;                ; affected meaning of undeferred portions of body: ...
     ;; But this must not:
     (define list cons)
     (list-macro 1 2))   ;==> (1 . 2)
   
   ;;(let ()
   ;;  (define-syntax macro
   ;;    (let ((x `(1 ,2)))
   ;;      (lambda (form) x)))
   ;;  (define unquote 2)
   ;;  (macro))    ; Syntax violation: Definition of identifier that may have already
   ;;              ; affected meaning of undeferred portions of body: unquote

   ;; Have to make sure that previous does give violation but this does not.
   (let ()
     (define-syntax macro
       (let ((x `(+ ,2)))
         (lambda (form) (cadr x))))
     (define + 2)
     (macro))    ;==> 2
 
   ;;======================================================
   ;;
   ;; Eval:
   ;;
   ;;====================================================== 
   
   (import (rnrs eval))
   
   (eval '(+ 1 2) 
         (environment '(rnrs)))   ;==> 3

   ;; This gives a syntax error as required by r6rs.
   
   ;;(eval '(begin (define x 1) x)
   ;;      (environment '(rnrs)))
   ;;                    ==>  Syntax violation: define
   ;;                         Invalid form in expression sequence
   ;;                         Form: (define x 1)
   
   (library (foo)
     (export foo-x foo-y)
     (import (rnrs))
     (define foo-x 4)
     (define-syntax foo-y (syntax-rules () ((_) 22))))
   
   (eval '(+ 1 (let-syntax ((foo (lambda (_) (+ foo-x (foo-y)))))
                 (foo)))
         (environment '(rnrs) '(for (foo) expand)))      ;==> 27
   
   (library (bar) 
     (export)
     (import (rnrs)
             (rnrs eval))
     
     (display
      (eval '(+ 1 (let-syntax ((foo (lambda (_) foo-x)))
                    (foo)))
            (environment '(rnrs) '(for (foo) expand)))))
   
   (import (bar))   ;==> 5
     
   ;;======================================================
   ;;
   ;; General syntax-case expander tests:
   ;;
   ;;======================================================
   
   (import (for (rnrs) run expand (meta 2)))
   
   (let-syntax ((m (lambda (e) 
                     (let-syntax ((n (lambda (e) 3)))
                       (n)))))
     (m))   ;==> 3
   
   ;; Some simple patern and template pitfalls:
   
   (syntax-case '((1 2) (3 4)) () 
     (((x ...) ...) (syntax (x ... ...))))     ;==> (1 2 3 4)
   
   ;; rnrs pattern extensions:
   
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
   
   (import (rnrs mutable-pairs))
   
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
   ;;(set! p.car 15)  ;==> Syntax violation: Keyword being set! is not a variable transformer
   
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
   ;;                                Binding is valid at levels: 1
    
   
   ;; Testing toplevel forward references:
   
   (define (f) (g))
   (define (g) 15)
   (f)             ;==> 15
   
   (define-syntax foo (lambda (_) (syntax (bar))))
   (define-syntax bar (lambda (_) 1))
   (foo)           ;==> 1
   
   ;; The following must give an error, since g-0 is bound at level 0 but used at level 1: 
   
   ;; (define-syntax foo (lambda (_) (g-0)))  ;==> Syntax violation: invalid reference
   ;;                                         ;    No binding available for g-0 at level 1  
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
     (import (rnrs))
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
   (tree-left t)         ;==> #(tree 0 1)
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
     ((d a) 'yes))  ;==> yes
   
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

   ;; QUASIQUOTE tests:

   `(list ,(+ 1 2) 4)                                ;==> (list 3 4)

   (let ((name 'a)) `(list ,name ',name))            ;==> (list a (quote a))

   `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)             ;==> (a 3 4 5 6 b)

   `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) ;==> ((foo 7) . cons)

   `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)         ;==> #(10 5 2 4 3 8)

   (let ((name 'foo))
     `((unquote name name name)))                    ;==> (foo foo foo)

   (let ((name '(foo)))
     `((unquote-splicing name name name)))           ;==> (foo foo foo)

   (let ((q '((append x y) (sqrt 9))))
     ``(foo ,,@q))              ;==> `(foo (unquote (append x y) (sqrt 9)))

   (let ((x '(2 3))
         (y '(4 5)))
     `(foo (unquote (append x y) (sqrt 9)))) ;==> (foo (2 3 4 5) 3)

   `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)  ;==>  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

   (let ((name1 'x)
         (name2 'y))
     `(a `(b ,,name1 ,',name2 d) e))         ;==>  (a `(b ,x ,'y d) e)
   
   ;; Test control library

   (import (rnrs control))

   (when (> 3 2) 'greater)       ;==> greater
   (when (< 3 2) 'greater)       ;==> unspecified
   (unless (> 3 2) 'less)        ;==> unspecified
   (unless (< 3 2) 'less)        ;==> less

   (do ((vec (make-vector 5))
        (i 0 (+ i 1)))
       ((= i 5) vec)
     (vector-set! vec i i))              ;==>  #(0 1 2 3 4)
     
   (let ((x '(1 3 5 7 9)))
     (do ((x x (cdr x))
          (sum 0 (+ sum (car x))))
         ((null? x) sum)))             ;==>  25
   
   (define foo
     (case-lambda 
       (() 'zero)
       ((x) (list 'one x))
       ((x y) (list 'two x y))
       ((a b c d . e) (list 'four a b c d e))
       (rest (list 'rest rest))))
   
   (foo)         ;==> zero
   (foo 1)       ;==> (one 1)
   (foo 1 2)     ;==> (two 1 2)
   (foo 1 2 3)   ;==> (rest (1 2 3))
   (foo 1 2 3 4) ;==> (four 1 2 3 4 ())

   ;;;=====================================================================
   ;;;
   ;;; (RNRS R5RS): scheme-report-environment and null-environment
   ;;;
   ;;;=====================================================================
   
   (program 
    (import (rnrs base)
            (rnrs io simple)
            (rnrs eval)
            (rnrs r5rs))
    
    (display
     (eval '(let ((x 1)) x)
           (null-environment 5)))  ;==> 1
    
    (display
     (eval '(let ((x (+ 1 2 3 4)))
              (list x x))
           (scheme-report-environment 5)))  ;==> (10 10)
    
    (let ((x (delay (begin (display 'boo)))))
      (force x)
      (force x))   ;==> displays "boo" once
    )
   
   )) ;; repl
