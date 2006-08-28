;;=========================================================================
;;
;; SRFI-83 (R6RS) LIBRARIES: Description, examples and tests.
;; ==========================================================
;;
;; The semantics is as in SRFI-83, but has been extended to
;; allow import into higher phases (as is need when let-syntax
;; is nested.
;;
;; The syntax for the latter is:
;;
;;   (for <import-set> <import-phase>*)
;;
;; where
;;
;;   <import-phase> ::= run | expand | all | 0 | 1 | 2 | ...
;;
;; Here 0 == run, 1 == expand, and |all| wil cause import into
;; all phases, which is implicitly done with the <language>.
;; See below for examples of use.
;;
;; The semantics is as follows:
;;
;; To invoke a library at phase N:
;;
;;    * Invoke at phase N any library that is imported by this library
;;      for run time, and that is not yet invoked at phase N. 
;;    * Evaluate all variable definitions and top-level expressions within
;;      the library. (Macro definitions are not evaluated.)
;;
;; To visit a library at phase N:
;;
;;    * For each k >= 1, invoke at phase N+k any library that is imported
;;      by this library for .... (phase k), and that is not yet invoked at
;;      phase N+k.
;;    * For each k >= 0, visit at phase N+k any library that is imported by
;;      this library for .... (phase k), and that is not yet visited at phase
;;      N+k.
;;    * Evaluate all syntax definitions within the library.
;;      (Top-level expressions are not evaluated, and the right-hand sides
;;      of variable definitions are not evaluated.)
;;
;; Technical remarks:
;; ==================
;;
;; * We do not enforce the no-shadowing and no-multiple-import
;;   constraints.
;; * We allow different bindings for the same identifier to be
;;   imported into different phases.
;; 
;; See also simple-syntax-case.scm for a non-trivial
;; example of libraries.
;;
;;=========================================================================

 

(load "simple-macros.scm")

(repl '
 (
  ;;================================================
  ;;
  ;; The SRFI-83 examples: 
  ;;
  ;;================================================
  
  (library "stack" "scheme://r6rs"
    
    (export make push! pop! empty!)
    
    (define (make)      (list '()))
    (define (push! s v) (set-car! s (cons v (car s))))
    (define (pop! s)    (let ((v (caar s)))
                          (set-car! s (cdar s))
                          v))
    (define (empty! s)  (set-car! s '())))
  
  
  (library "balloons" "scheme://r6rs"
    
    (export make push pop)
    
    (define (make w h)   (cons w h))
    (define (push b amt) (cons (- (car b) amt) (+ (cdr b) amt)))
    (define (pop b)      
      (display "Boom! ") 
      (display (* (car b) (cdr b))) 
      (newline)))
  
  
  (library "party" "scheme://r6rs" 
    
    (import (only "stack" make push! pop!) ; not empty!
            (add-prefix "balloons" balloon:))
    
    ;; Total exports: make, push, 
    ;;                push!,
    ;;                make-party,
    ;;                pop!, pop-again!
    
    (export (rename (balloon:make make)
	            (balloon:push push))
	    push!
	    make-party
	    (rename (party-pop! pop!)))
    
    ;; Creates a party as a stack of balloons, starting with
    ;; two balloons
    
    (define (make-party)
      (let ((s (make))) ; from stack
        (push! s (balloon:make 10 10))
        (push! s (balloon:make 12 9))
        s))
    
    (define (party-pop! p)
      (balloon:pop (pop! p))))
  
  
  (library "main" "scheme://r6rs"
    
    (import "party")
    
    (define p (make-party))
    (pop! p)       
    (push! p (push (make 5 5) 1))
    (pop! p))      
  
  (import "main")   ; displays "Boom! 108"
                    ; displays "Boom! 24"
  

  
  ;; Examples for macros and phases:
  
  (library "helper" "scheme://r6rs" 
    
    (export find-dup)
    
    (define (find-dup l)
      (and (pair? l)
           (let loop ((rest (cdr l)))
             (cond
               ((null? rest)                            
                (find-dup (cdr l)))
               ((bound-identifier=? (car l) (car rest)) 
                (car rest))
               (else (loop (cdr rest))))))))
  
  
  (library "let-values" "scheme://r6rs" 
    
    (import (for "helper" expand)) 
    
    (export let-values)
    
    (define-syntax let-values
      (lambda (stx)
        (syntax-case stx ()
          ((_ ((id ...) expr) body0 body ...)
           (not (find-dup (syntax (id ...))))
           (syntax 
            (call-with-values (lambda () expr) 
                              (lambda (id ...) body0 body ...))))))))
  
  
  (library "let-div" "scheme://r6rs"
    
    (import "let-values")
    
    (export let-div)
    
    (indirect-export (let-div quotient+remainder))
    
    (define (quotient+remainder n d)
      ;; recompute, for now...
      (values (quotient n d) (remainder n d)))
    
    (define-syntax let-div
      (syntax-rules ()
        ((_ n d (q r) body0 body ...)
         (let-values ((q r) (quotient+remainder n d))
           body0 body ...)))))
  
  (import "let-div")
  
  (let-div 11 5 (q r) (values q r))   ;==> 2 1 
  

  
  ;;====================================================
  ;;
  ;; Importing into higher phases:  
  ;;
  ;;====================================================
  
  
  (library "m" "scheme://r6rs"
    (export x)
    (define x 1))
  
  (library "n" "scheme://r6rs"
    (export y)
    (define y 2))
  
  (library "o" "scheme://r6rs"
    (export z)
    (define z 3))

  (library "p" "scheme://r6rs"
    (import "m")                ; same as (for "m" 0)
    (import (for "n" expand))   ; same as (for "n" 1)
    (import (for "o" 2))        
    
    (let-syntax ((foo (lambda (form)
                        (let-syntax ((bar (lambda (form)
                                            (quasisyntax
                                             (quasisyntax (list x ,y ,,z))))))
                          (bar)))))
      (display (foo))))
  
  (import "p")  ;==> (1 2 3)



  ;;====================================================
  ;;
  ;; Matthew Flatt's example from the paper:
  ;;
  ;;====================================================
  
  (library "records" "scheme://r6rs"

    (export define-record
            record-ref)
    
    (import (for "syntax-case" expand))
    
    (begin-for-syntax 
      (display "Creating fresh table")         ;==> Creating fresh table
      (newline)
      (define registry '())
      
      (define (register name fields)
        (if (assq name registry)
            (syntax-error "Duplicate record type: " name)) 
        (display "Registering: ") (display name)
        (newline)
        (set! registry (cons (cons name fields) registry)))
      
      ); begin-for-syntax
    
    (define-syntax define-record 
      (syntax-rules ()
        ((define-record name pred? field ...)
         (begin
           (begin-for-syntax 
             (register 'name '(field ...)))
           (define (pred? x) (and (pair? x) (eq? (car x) 'name)))
           (define (name field ...)
             (list 'name field ...))))))
    
    (define-syntax record-ref
      (lambda (form)
        (syntax-case form ()
          ((_ exp name field)
           (let ((entry (assq (syntax-object->datum (syntax name)) registry)))
             (if entry
                 (let ((maybe-member (member (syntax-object->datum (syntax field))
                                             entry)))
                   (if maybe-member
                       (with-syntax
                           ((index (- (length entry) (length maybe-member))))
                         (syntax
                          (list-ref exp index)))
                       (syntax-error "Unknown field" (syntax field))))
                 (syntax-error "Unknown record type" (syntax name))))))))
    ) ; records
  
  
  (library "savannah" "scheme://r6rs"

    (export giraffe giraffe? lion lion?)
    
    (import "records")                                     ;==> Creating fresh table
    (define-record giraffe giraffe? height speed weight)   ;==> Registering: giraffe
    (define-record lion    lion?    speed weight))         ;==> Registering: lion  
  
  
  (library "metrics" "scheme://r6rs"

    (export weight)
    
    (import "records")                                     ;==> Creating fresh table
    (import "savannah")                                    ;==> Registering: giraffe
                                                           ;==> Registering: lion 
    (define (weight animal)
      (cond ((lion? animal)    (record-ref animal lion weight))
            ((giraffe? animal) (record-ref animal giraffe weight)))))
  
  (library "main" "scheme://r6rs"
    (import "savannah")                                    ;==> Creating fresh table
                                                           ;==> Registering: giraffe
                                                           ;==> Registering: lion
    (import "metrics")
    (display (weight (giraffe 25 51 1000))))    
  
  (import "main")                                          ;==> Creating fresh table
                                                           ;==> Registering: giraffe
                                                           ;==> Registering: lion
                                                           ;==> 1000
  


  
  
  ;;==============================================
  ;;
  ;; Check that visits are correctly chained: 
  ;;
  ;;==============================================

  (library "m" "scheme://r6rs"
    (export u)
    (define-syntax u (lambda (exp) (syntax (syntax 1)))))

  (library "n" "scheme://r6rs"
    (import (for "m" expand))
    (export v)
    (define-syntax v (lambda (exp) (u))))

  (import "n")
  (v)           ;==> 1


  
  ;;=====================================================
  ;;
  ;; In the following example, 'm is only displayed once
  ;; even though m is imported into o via two different
  ;; import chains.
  ;;
  ;;=====================================================
  
  (library "m" "scheme://r6rs"
    (display 'm))   
  
  (library "n" "scheme://r6rs" 
    (import "m")     
    (display 'n))   
  
  (library "o" "scheme://r6rs"    
    (import "n")     
    (import "m"))            
  
  (import "o")        ;==> m n


  
  ;;======================================================================
  ;;
  ;; In the following example, (import (for "m" expand)) causes 
  ;; m to be visited and invoked at phase 1.
  ;; Then (import "m") causes m to be visited at phase 0.
  ;;
  ;;======================================================================
  
  (library "m" "scheme://r6rs"
    (begin-for-syntax (display 'm-visit))  ;==> m-visit
    (display 'm-invoke))
  
  (library "n" "scheme://r6rs"
    (import (for "m" expand))           ;==> m-visit m-invoke
    (import "m"))                       ;==> m-visit


  
  ;;======================================================================
  ;;
  ;; Technical point:
  ;;
  ;; We do not constrain to single import of a binding:
  ;;
  ;; Different bindings for the same identifier can be
  ;; imported into different phases of the reflective tower.  
  ;;
  ;;======================================================================

  
  (library "m" "scheme://r6rs"
    (export x)
    (define x 1))
  
  (library "n" "scheme://r6rs"
    (import (for "m" expand))
    (define x 2)
    (display (let-syntax ((m (lambda (_) x)))
               (m)))
    (display x)
    ) ; n
  
  (import "n")  ;==> 1 2


  ;; Another example, where we import into phase 0, 1 AND 2
  
  (library "m" "scheme://r6rs"
    (export x)
    (define x 1))
  
  (library "n" "scheme://r6rs"
    (export x)
    (define x 2))
  
  (library "o" "scheme://r6rs"
    (export x)
    (define x 3))
  

  (library "p" "scheme://r6rs"
    (import "m")
    (import (for "n" expand))
    (import (for "o" 2))
    
    (let-syntax ((foo (lambda (form)
                        (let-syntax ((bar (lambda (form)
                                            (quasisyntax
                                             (quasisyntax (list x ,x ,,x))))))
                          (bar)))))
      (display (foo))))
  
  (import "p")  ;==> (1 2 3)

 

  ;;======================================================================
  ;;
  ;; Technical point:
  ;; Only phase 0 bindings are exported.
  ;;
  ;;======================================================================
  
  (library "m" "scheme://r6rs"
    (export x y)
    (define x 1)
    (begin-for-syntax (define x 2))
    (begin-for-syntax (define y 3)))
  
  (import "m")
  x                        ;==> 1
  ; (begin-for-syntax y)   ;==> reference to unidentified identifier: y
  

  ;;======================================================================
  ;;
  ;; Technical point:
  ;; Since we allow imports to be shadowed, and since we
  ;; can export unbound identifiers, we can
  ;; removing bindings from an environment as follows:
  ;;
  ;;=====================================================================
  
  (library "remove-x" "scheme://r6rs"
    (export x))
  
  (define x 1)     
  x                  ;==> 1
  (import "remove-x")  
  ; x                ;==> reference to unidentified identifier: x

  
  
  ;;======================================================================
  ;;
  ;; Interaction with begin-for-syntax:
  ;;
  ;; In the following example, u is available during compilation
  ;; at phase 1 for expanding the right hand side of v, while
  ;; v is available during compilation at phase 0 for
  ;; expanding the argument of display. The display statement is
  ;; compiled but not evaluated until the library is later imported:
  ;;
  ;;======================================================================
  
  (library "m" "scheme://r6rs"
    
    (begin-for-syntax
      (define-syntax u (lambda (form) (syntax 1))))
    
    (define-syntax v (lambda (form) (quasisyntax (list ,(u) 2))))
    
    (display (v)))
  
  (import "m")  ;==> (1 2)
  
  
  ;;======================================================================
  ;;
  ;; In the following example, the code wrapped in begin-for-syntax is
  ;; evaluated when m is imported for syntax during
  ;; expansion of n, which imports it directly, and during expansion
  ;; of o, which imports it indirectly, and again when the toplevel
  ;; import form is expanded. The expanded library bodies are not executed
  ;; until the final toplevel import form is evaluated.
  ;;
  ;;======================================================================
  
  (library "m" "scheme://r6rs"
    (begin-for-syntax
      (display 'm-visit))       ;==> m-visit
    (display 'm-invoke))     
  
  (library "n" "scheme://r6rs"
    (import "m"))               ;==> m-visit
  
  (library "o" "scheme://r6rs"
    (import "n")                ;==> m-visit
    (import "m"))               
  
  (import "o")                  ;==> m-visit m-invoke
  
  

  
  ;;======================================================================
  ;;
  ;; Just another example:
  ;;
  ;;======================================================================
  
  
  (library "m" "scheme://r6rs"

    (export v)
    
    (indirect-export (v g))
    
    (begin-for-syntax 
      (define-syntax u (lambda (form) (syntax 1))))
    
    (define-syntax v (lambda (form) (quasisyntax (g ,(+ (f) (u))))))
    (begin-for-syntax 
      (define (f) 2))
    
    (define (g n) (/ n))
    
    ) ; m
  
  (import "m")
  
  (v)        ;==> 1/3


  
  ;;======================================================================
  ;;
  ;; Convenience for importing into all levels:
  ;;
  ;;======================================================================
  
  (library "m" "scheme://r6rs"
    (export z)
    (define z 1))
  
  (import (for "m" all))  ; same as (import (for "m" 0 1 2 ...))
  
  (let-syntax ((foo (lambda (form)
                      (let-syntax ((bar (lambda (form)
                                          (quasisyntax
                                           (quasisyntax (list z ,z ,,z))))))
                        (bar)))))
    (foo))   
               ;==> (1 1 1)




  ))
  
  
  
