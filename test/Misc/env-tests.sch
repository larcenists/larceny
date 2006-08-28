; Copyright 2000 Lars T Hansen
;
; $Id$
;
; Test code for how the syntactic environments and compiler macros are
; used by Larceny.
;
; 2000-09-27 / lth
;
; Test by loading this file, UNCOMPILED, into a production heap.
;
; The code won't work if it's loaded into the bootstrap heap because it
; tests some features of the compiler, and it won't work if it's
; compiled by COMPILE-FILE because of the way DEFINE-SYNTAX is handled
; (syntax definitions disappear from the compiled file).
;
; That's a minor shortcoming -- this file tests COMPILE-FILE in Part 3.

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(define (identity x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part 1: user macros.

; Test: does the standard environment contain only the expected macros?

(or (equal? (environment-macros (scheme-report-environment 4))
            '(quote lambda if set! begin define define-syntax let-syntax
              letrec-syntax let*-syntax let let* letrec and or cond do
              case quasiquote syntax-rules))
    (fail 'standard-macros))

; Test: does a new environment contain no macros?

(or (equal? (environment-macros (make-environment "empty"))
            '())
    (fail 'no-macros))

; Test: does macro transfer work?

(let ((e (make-environment "new"))
      (f (interaction-environment)))
  (environment-set-macro! e 'define-syntax 
                          (environment-get-macro f 'define-syntax))
  (environment-set-macro! e 'syntax-rules
                          (environment-get-macro f 'syntax-rules))
  (or (equal? (environment-macros e)
              '(define-syntax syntax-rules))
      (fail 'copied-macros)))

; Test: does define-syntax define a macro in the interaction environment?

(define-syntax test1
  (syntax-rules ()
    ((test1 x) x)))

(or (memq 'test1 (environment-macros (interaction-environment)))
    (fail 'define-syntax:1a))

; Test: does the macro work?

(or (= (identity 33) (test1 33))
    (fail 'define-syntax:1b))

; Test: does define-syntax with an explicit environment define a macro
; in the given environment, and leave the interaction environment untouched?  
; And does that macro work?

(let ((e (make-environment "test2"))
      (f (interaction-environment)))
  (environment-set-macro! e 'define-syntax 
                          (environment-get-macro f 'define-syntax))
  (environment-set-macro! e 'syntax-rules
                          (environment-get-macro f 'syntax-rules))
  (eval '(define-syntax test2 
           (syntax-rules ()
             ((test2 x) x)))
        e)

  ; definition OK?
  (or (and (memq 'test2 (environment-macros e))
           (not (memq 'test2 (environment-macros f))))
      (fail 'define-syntax:2a))

  ; use OK?
  (environment-set-macro! e 'lambda (environment-get-macro f 'lambda))
  (or (= ((eval '(lambda (f) (f (test2 33))) e) identity)
         33)
      (fail 'define-syntax:2b)))

; Test: does redefinition of macros work, and is redefinition to EVAL
; reflected in answers received from introspection interface, and vice
; versa?

(define saved-let-syntax (environment-get-macro (interaction-environment)
                                                'let-syntax))

(define let-syntax 37)  ; who needs it anyway... :-)

(or (= (+ let-syntax 10) 47)
    (fail 'redefinition:1))

(or (and (not (memq 'let-syntax (environment-macros 
                                 (interaction-environment))))
         (memq 'let-syntax (environment-variables (interaction-environment))))
    (fail 'redefinition:2))

(environment-set-macro! (interaction-environment) 'let-syntax saved-let-syntax)

(or (and (memq 'let-syntax (environment-macros 
                            (interaction-environment)))
         (not (memq 'let-syntax (environment-variables
                                 (interaction-environment)))))
    (fail 'redefinition:3))

; Does let-syntax still work?

(or (= (let-syntax ((glarg (syntax-rules ()
                             ((glarg x) (* x x)))))
         (glarg (glarg 2)))
       16)
    (fail 'redefintion:4))

; The following had better fail, but the reason for the failure depends 
; on whether the compiler 
;  (a) signals an error [ it may ]
;  (b) signals a warning and generates buggy code [ 0.50 does ]
;  (c) generates correct code that fails to link [ it should ]
;  (d) generates correct code that links and fails to run with an
;      undefined variable error [ it shouldn't ]
;  (e) generates incorrect code that produces an erroneous result 
;      but fails to fail [ it shouldn't ]
; Anyway, just require an error here...

(or (equal? (call-with-current-continuation 
             (lambda (k)
               (parameterize ((error-handler (lambda args (k "error"))))
                 (eval '(+ let-syntax 10)))))
            "error")
    (fail 'redefinition:5))

; Go at it from the other direction

(environment-set! (interaction-environment) 'let-syntax 22/7)

(or (and (not (memq 'let-syntax (environment-macros 
                                 (interaction-environment))))
         (memq 'let-syntax (environment-variables (interaction-environment))))
    (fail 'redefinition:6))

; Did EVAL observe that let-syntax is no longer a macro?

(or (equal? (call-with-current-continuation 
             (lambda (k)
               (parameterize ((error-handler (lambda args (k "error"))))
                 (eval '(+ let-syntax 10)))))
            92/7)
    (fail 'redefinition:7))

; Clean up

(environment-set-macro! (interaction-environment) 'let-syntax saved-let-syntax)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part 2: Compiler macros.

; Test: test that switches work and that procedures that should be inlined
; are inlined

; Using 'vector' below because it won't be inlined unless the compiler
; macros kick in -- whereas cons is a primitive that can be open-coded
; without compiler macros.

(parameterize ((integrate-procedures 'larceny))
  (let ((e (make-environment "new"))
        (f (interaction-environment)))
    (environment-set-macro! e 'lambda (environment-get-macro f 'lambda))
    (environment-set! e 'fac (letrec ((fac
                                       (lambda (n)
                                         (if (< n 2) 1 (* n (fac (- n 1)))))))
                               fac))
    (or (equal? ((eval '(lambda (a b) (vector (fx+ a b) (fac 3))) e) 3 4) 
                '#(7 6))
        (fail 'inline:larceny))))

; Redefinition of fx+ tests that we do not get an inlined version.

(parameterize ((integrate-procedures 'r4rs)) ; == r5rs right now
  (let ((e (make-environment "new"))
        (f (interaction-environment)))
    (environment-set-macro! e 'lambda (environment-get-macro f 'lambda))
    (environment-set! e 'fx+ (lambda (a b) (* a b)))
    (environment-set! e 'fac (letrec ((fac
                                       (lambda (n)
                                         (if (< n 2) 1 (* n (fac (- n 1)))))))
                               fac))
    (or (equal? ((eval '(lambda (a b) (vector (fx+ a b) (fac 3))) e) 3 4) 
                '#(12 6))
        (fail 'inline:r4rs))))

; Redefinition of fx+ tests that we do not get an inlined version.
; Reversal of args to vector tests that we do not get an inlined version.

(parameterize ((integrate-procedures 'none))
  (let ((e (make-environment "new"))
        (f (interaction-environment)))
    (environment-set-macro! e 'lambda (environment-get-macro f 'lambda))
    (environment-set! e 'fx+ (lambda (a b) (* a b)))
    (environment-set! e 'vector (lambda (a b) (vector b a)))
    (environment-set! e 'fac (letrec ((fac
                                       (lambda (n)
                                         (if (< n 2) 1 (* n (fac (- n 1)))))))
                               fac))
    (or (equal? ((eval '(lambda (a b) (vector (fx+ a b) (fac 3))) e) 3 4) 
                '#(6 12))
        (fail 'inline:none))))

; Observe that inlined primitives are still available in null
; environments, that is, the selection of environment does not impact
; inlines

(parameterize ((integrate-procedures 'larceny))
  (or (string=? ((eval '(lambda (x) (car x)) (null-environment 5)) 
                 (cons "success" #f))
                "success")
      (fail 'inline:primitives-available)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part 3: compile-file
;
; Here we test that:
;  - compile-file does not affect the current interaction environment
;  - compile-file uses the current interaction environment as context

(define filename "$$test.sch")
(define faslname "$$test.fasl")

(with-output-to-file filename
  (lambda ()
    (for-each 
     pretty-print 
     '((define-syntax whatever
         (syntax-rules ()
           ((whatever ?c1 ?c2)
            (if (zero? (random 1))
                ?c1
                ?c2))))

       ; Is the macro visible inside file?
       (or (= (whatever 10 20) 10)
           (fail 'whatever:1))

       ; Is the macro WHENEVER, defined in the compilation
       ; environment, visible here?
       (or (= (whenever 10 20) 20)
           (fail 'whenever:1))

       ; Evaluates to 0 if integrate-procedures = larceny
       ;           to 1 if integrate-procedures = r5rs/r5rs
       ;           to 2 if integrate-procedures = none

       (define fx+ (lambda (a b) 1))
       (define cons (lambda (a b) '(1 . 0)))

       (set! inferred-compiler-switches
             (+ (fx+ (identity 0) (identity 0))
                (car (cons (identity 0) #f))))
       
       #t))))

(define-syntax whenever
  (syntax-rules ()
    ((whenever ?c1 ?c2)
     (if (zero? (random 1))
         ?c2
         ?c1))))

(parameterize ((integrate-procedures 'larceny))
  (compile-file filename)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:1:1))
  (load faslname)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:1:2))
  (or (= 0 inferred-compiler-switches)
      (fail 'compile-file:1:3)))

(parameterize ((integrate-procedures 'r4rs))
  (compile-file filename)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:2:1))
  (load faslname)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:2:2))
  (or (= 1 inferred-compiler-switches)
      (fail 'compile-file:2:3)))

(parameterize ((integrate-procedures 'none))
  (compile-file filename)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:3:1))
  (load faslname)
  (or (not (memq 'whatever (environment-macros (interaction-environment))))
      (fail 'compile-file:3:2))
  (or (= 2 inferred-compiler-switches)
      (fail 'compile-file:3:3)))

(delete-file filename)
(delete-file faslname)

; eof
