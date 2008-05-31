(define (fresh-assembly-structure)
  (make-assembly-structure #f (assembly-table) (assembly-user-data)))

(define (quick-check)
  (let ((as (fresh-assembly-structure))
	(slot0 (lcg:field:offset->slot 0))
	(sym->op lcg:sym->opcode))
    (assembly-start as)
    (make-asm-label as 0)
    (ilgen-msg! as 'emit-fieldinfo  (sym->op 'ldsfld) lcg:field:cont)
    (ilgen-msg! as 'emit-fieldinfo  (sym->op 'ldfld)  slot0)
    (ilgen-msg! as 'emit-fieldinfo  (sym->op 'ldfld)  lcg:field:entrypoint)
    (ilgen-msg! as 'emit-fieldinfo  (sym->op 'ldsfld) lcg:field:cont)
    (ilgen-msg! as 'emit-fieldinfo  (sym->op 'ldfld)  lcg:field:returnIndex)
    (ilgen-msg! as 'emit-methodinfo (sym->op 'call)   lcg:method:call)
    (ilgen-msg! as 'emit-type       (sym->op 'castclass) lcg:type:code-address)
    (ilgen-msg! as 'emit            (sym->op 'ret))
    (assembly-end as #f)
    (link-lop-segment 
     (cons (clr/%foreign->schemeobject 
	    (lcg/%invoke (eval (string->symbol "lcg:method:makeCodeVector")) 
			 #f 
			 (vector (clr/int->foreign 1) (as.dyn-meth as)))) 
	   '#((data #f))) (interaction-environment))))

(define (tc x) (pretty-print (readify-lap (compile x))))

(define (uniquify syms)
  (cond 
   ((null? syms) '())
   ((memq (car syms) (cdr syms))
    (uniquify (cdr syms)))
   (else (cons (car syms) (uniquify (cdr syms))))))

(define (instructions-of x)
  (let ((compiled (readify-lap (compile x))))
    (letrec ((instructions (lambda (is)
			     (apply append (map instruction is))))
	     (instruction (lambda (i)
			    (case (car i)
			      ((lambda) (cons (car i)
					      (instructions (cadr i))))
			      ((lexical return .proc args= reg setreg
				op2 branch .align .label .proc-doc
				op2imm branchf const invoke save
				store op1 movereg jump load pop)
			       (list (car i)))
			      (else (error "unknown " (car i)))))))
      (instructions compiled))))


(define examples
  (letrec-syntax ((eg (syntax-rules (eql)
			((_ EXPR SETUP eql EXPECTED)
			 (eg EXPR SETUP r (equal? r EXPECTED)))
			((_ EXPR SETUP res-id CHECK-RES)
			 (eg EXPR SETUP (lambda (res-id) CHECK-RES)))
			((_ EXPR SETUP CHECK)
			 (list 'EXPR (lambda () SETUP) CHECK))
			)))
    (let ((pure-identity? (lambda (fcn)
			    (let ((x (cons 1 2))
				  (y (cons 3 4)))
			      (and (eq? (fcn x) x) 
				   (eq? (fcn y) y) 
				   (eq? (fcn (fcn x)) x)))))
	  (twice-of-2? (lambda (fcn)
			 (and (equal? (fcn (lambda (x) (+ x 1))) 4)
			      (equal? (fcn (lambda (x) (- x 1))) 0)
			      (equal? (fcn (lambda (x) (* x 3))) 18))))
	  (sub-x-from-y? (lambda (fcn)
			   (equal? (fcn 3 7) 4)))
	  (cur-x-from-y? (lambda (fcn)
			   (equal? ((fcn 3) 7) 4))))
      (list
       ;; 0-3
       (eg 0          'no-setup                     eql 0)
       (eg 1          'no-setup                     eql 1)
       (eg x          (set! x '(a b c))             eql '(a b c))
       (eg 10         'no-setup                     eql 10)

       ;; 4-7
       (eg (if x 1 2) (set! x #t)                   eql 1)
       (eg (car x)    (set! x '(a b c))             eql 'a)
       (eg (+ x y)    (begin (set! x 4) (set! y 5)) eql 9)
       (eg (+ x 10)   (set! x 17)                   eql 27)
       
       ;; 8-11
       (eg (vector-set! x y z) 
	   (begin (set! x (list->vector '(a b c d e f g h i j k l m n o))) 
		  (set! y 5) 
		  (set! z 'z))
	   unspecified-result
	   (equal? x (list->vector '(a b c d e z g h i j k l m n o))))
       (eg (lambda vrs 10)                   'no-setup fcn (and (equal? (fcn) 10) (equal? (+ 1 (fcn)) 11)))
       (eg (lambda (x) x)                    'no-setup pure-identity?)
       (eg (lambda (x y) (- y x))            'no-setup sub-x-from-y?)

       ;; 12-15
       (eg (lambda (x) (lambda (y) (- y x))) 'no-setup cur-x-from-y?)
       (eg (lambda (x) 
	     (letrec ((even? (lambda (x) (if (= x 0) #t (odd? (- x 1))))) 
		      (odd? (lambda (x) (if (= x 0) #f (even? (- x 1))))))
	       (lambda (y) (even? (- y x)))))
	   'no-setup
	   fcn
	   (equal? ((fcn 3) 7) #t))
       (eg (let* ((f (lambda (x) x))
		  (g (lambda (y) y)))
	     (lambda (z) ((f g) z)))
	   'no-setup
	   pure-identity?)
       (eg (let ((f (lambda (y) y)))
	     (lambda (z)
	       f))
	   'no-setup
	   fcn
	   (pure-identity? (fcn 'ignored)))

       ;; 16-19
       (eg (lambda (f) (f (f 2)))
	   'no-setup
	   twice-of-2?)
       (eg (let ((f (lambda (y) y)))
	     (lambda (z)
	       ((f f) z)))
	   'no-setup
	   pure-identity?)
       (eg (lambda (x)
	     (let ((f (lambda (y) y)))
	       (lambda (z)
		 ((f f) z))))
	   'no-setup
	   fcn
	   (pure-identity? (fcn 'ignored)))
       (eg (lambda (a) 
	     (letrec ((even? (lambda (x) (if (= x 0) #t (odd? (- x 1))))) 
		      (odd? (lambda (y)  (if (= y 0) #f (even? (- y 1)))))
		      (app (lambda (f z) (f (- z a)))))
	       (lambda (b) (app even? b))))
	   'no-setup
	   fcn
	   (equal? ((fcn 3) 7) #t))
       
       ;; 20-
       (eg (define f (lambda (x) x))
	   (set! f (undefined))
	   (lambda (ignored)
	     (pure-identity? f)))
       (eg (define (fib n)
	     (case n
	       ((0 1) 1)
	       (else (+ (fib (- n 1)) (fib (- n 2))))))
	   (set! fib (undefined))
	   (lambda (ignored)
	     (and (equal? (fib 10) 89)
		  (equal? (fib 25) 121393))))

       ))))

(define e (lambda (idx) (car (list-ref examples idx))))
(define c (lambda (scm) (compile scm)))
(define a (lambda (mal) (assemble mal)))
(define l (lambda (lop) (link-lop-segment lop (interaction-environment))))

(define (test-example idx . rest)
  (let* ((noisy   (memq 'noisy rest))
	 (example (list-ref examples idx))
	 (code    (list-ref example 0))
	 (setup   (list-ref example 1))
	 (check   (list-ref example 2)))
    (cond (noisy (display "example: ") (display code) (newline)))
    (cond (noisy (display 'setup) (newline)))
    (setup)
    (cond (noisy (display 'compile) (newline)))
    (let ((linked (l (a (c code)))))
      (cond (noisy (display 'execute) (newline)))
      (let ((result (linked)))
	(cond (noisy (display 'check) (newline)))
	(check result)))))

(define (load-asm-il-lcg)
  (load "src/Asm/IL-LCG/peepopt.sch")
  (load "src/Asm/IL-LCG/pass5p2.sch"))

;; Note that this definition gets overridden when it is invoked!
(define load-compiler
  (lambda ()  
    (load "lib/Debugger/inspect-cont.sch")
    (load "lib/Debugger/debug.sch")
    (load "lib/Debugger/trace.sch")
    (install-debugger)
    (load "src/Build/dotnet.sch")
    (larceny-setup "Larceny" 'win32 'little)
    (load-compiler)
    (peephole-optimization #f)
    (load-asm-il-lcg)
    (load "src/Asm/Shared/link-lop.sch")
    ))
