;; -*- scheme -*-
;;
;; Larceny -- Evaluator
;;
;; Small Scheme evaluator which provides a minimal interface to Larceny.
;; Typically one does not want to write programs at the interpreter prompt
;; but rather compile them and load the compiled code. This works. But one
;; can also load interpretable code (raw scheme) and have it work, slowly.
;;
;; $Id: eval.sch,v 1.7 1992/08/04 18:27:43 lth Exp $
;;
;; 'Eval' takes an expression and evaluates the expression in the default
;; toplevel environment. It returns the result of the evaluation,
;; and may change the environment.
;;
;; 'Eval' accepts full R4RS Scheme, but requires a procedure "rewrite" which
;; converts full Scheme to its core form.

(define eval-version "0.1")

(define eval
  (let ((ptag (vector '**proc**)))
    (lambda (expr)

      (define quote.const cadr)
      (define set.ident cadr)
      (define set.expr caddr)
      (define begin.exprs cdr)
      (define if.test cadr)
      (define if.then caddr)
      (define (if.else expr) (if (null? (cdddr expr)) #f (cadddr expr)))
      (define lambda.args cadr)
      (define lambda.body cddr)
      (define application.proc car)
      (define application.args cdr)

      ;; This is devious. It allows us to pass interpreted procedures
      ;; to compiled code...

      (define (make-proc env expr) 
	(lambda args
	  (eval (cons 'begin (lambda.body expr))
		(extend-env env (lambda.args expr) args))))

      (define (quotation? expr) (and (pair? expr) (eq? (car expr) 'quote)))
      (define (assignment? expr) (and (pair? expr) (eq? (car expr) 'set!)))
      (define (lambda? expr) (and (pair? expr) (eq? (car expr) 'lambda)))
      (define (begin? expr) (and (pair? expr) (eq? (car expr) 'begin)))
      (define (conditional? expr) (and (pair? expr) (eq? (car expr) 'if)))

      (define (constant? expr)
	(or (number? expr)
	    (boolean? expr)
	    (char? expr)
	    (string? expr)
	    (procedure? expr)))

      (define (eval expr env)
	(cond ((symbol? expr)
	       (env-lookup env expr))
	      ((constant? expr)
	       expr)
	      ((quotation? expr)
	       (quote.const expr))
	      ((assignment? expr)
	       (env-set! env (set.ident expr) (eval (set.expr expr) env)))
	      ((lambda? expr)
	       (make-proc env expr))
	      ((begin? expr)
	       (let loop ((exprs (begin.exprs expr)))
		 (cond ((null? exprs)
			(error "Empty BEGIN."))
		       ((null? (cdr exprs))
			(eval (car exprs) env))
		       (else
			(eval (car exprs) env)
			(loop (cdr exprs))))))
	      ((conditional? expr)
	       (let ((test (eval (if.test expr) env)))
		 (if test
		     (eval (if.then expr) env)
		     (eval (if.else expr) env))))
	      (else
	       (let ((args (map (lambda (x) (eval x env)) expr)))
		 (cond ((null? args)
			(error "Null procedure call or unquoted empty list."))
		       ((procedure? (application.proc args))
			(apply (application.proc args)
			       (application.args args)))
		       (else
			(error "Not a procedure")))))))

      (define (toplevel-eval expr)
	(if (and (pair? expr) (eq? (car expr) 'define))
	    (begin (toplevel-env-set!
		    (cadr expr)
		    (eval (caddr expr) (empty-env)))
		   (cadr expr))
	    (eval expr (empty-env))))

      (toplevel-eval (rewrite expr)))))


;; Environment stuff.

(define (empty-env) '())

(define (extend-env env n v)

  (define (extend-env env names values)
    (cond ((and (null? names) (null? values))
	   env)
	  ((null? names)
	   (error "Wrong number of arguments to procedure;" n v))
	  ((not (pair? names))
	   (cons (cons names values) env))
	  (else
	   (extend-env (cons (cons (car names) (car values)) env)
		       (cdr names)
		       (cdr values)))))

  (extend-env env n v))

(define (env-set! env name value)
  (let ((probe (assq name env)))
    (if probe
	(set-cdr! probe value)
	(toplevel-env-set! name value))))

(define (env-lookup env name)
  (let ((probe (assq name env)))
    (if probe
	(cdr probe)
	(toplevel-env-lookup name))))


;; The top-level environment.

;; Return the global variable cell for the named variable in the default
;; environment. Create cell if it does not exist.

(define (toplevel-cell symbol)
  (let ((cell (toplevel-env-find symbol)))
    (if cell
	cell
	(begin (extend-toplevel-env! symbol (unspecified))
	       (toplevel-cell symbol)))))

;; Return the value associated with the name, if any. Error if none.

(define (toplevel-env-lookup name)
  (let ((cell (toplevel-env-find name)))
    (if (or (not cell)
	    (eq? (global-cell-ref cell) (unspecified)))
	(error "Undefined or uninitialized global variable" name)
	(global-cell-ref cell))))

;; Change the binding of a toplevel name.

(define (toplevel-env-set! name value)
  (let ((cell (toplevel-env-find name)))
    (if cell
	(global-cell-set! cell value)
	(extend-toplevel-env! name value))))

(define make-global-cell cons)
(define global-cell-ref car)
(define global-cell-set! set-car!)


;; We use property lists for the global environment. This is just a hack; I'm
;; too lazy to implement a hash table, and this evaluator will eventually go
;; away, anyway.

(define (toplevel-env-find symbol)
  (getprop symbol 'value))

(define (extend-toplevel-env! name value)
  (putprop name 'value (make-global-cell value name)))

(define (init-toplevel-environment)

  ;; booleans

  (extend-toplevel-env! 'not not)
  (extend-toplevel-env! 'boolean? boolean?)

  ;; equivalence predicates

  (extend-toplevel-env! 'eq? eq?)
  (extend-toplevel-env! 'eqv? eqv?)
  (extend-toplevel-env! 'equal? equal?)

  ;; pairs and lists

  (extend-toplevel-env! 'pair? pair?)
  (extend-toplevel-env! 'cons cons)
  (extend-toplevel-env! 'car car)
  (extend-toplevel-env! 'cdr cdr)
  (extend-toplevel-env! 'set-car! set-car!)
  (extend-toplevel-env! 'set-cdr! set-cdr!)
  (extend-toplevel-env! 'caar caar)
  (extend-toplevel-env! 'cadr cadr)
  (extend-toplevel-env! 'cdar cdar)
  (extend-toplevel-env! 'cddr cddr)
  (extend-toplevel-env! 'caaar caaar)
  (extend-toplevel-env! 'caadr caadr)
  (extend-toplevel-env! 'cadar cadar)
  (extend-toplevel-env! 'caddr caddr)
  (extend-toplevel-env! 'cdaar cdaar)
  (extend-toplevel-env! 'cdadr cdadr)
  (extend-toplevel-env! 'cddar cddar)
  (extend-toplevel-env! 'cdddr cdddr)
  (extend-toplevel-env! 'caaaar caaaar)
  (extend-toplevel-env! 'caaadr caaadr)
  (extend-toplevel-env! 'caadar caadar)
  (extend-toplevel-env! 'caaddr caaddr)
  (extend-toplevel-env! 'cadaar cadaar)
  (extend-toplevel-env! 'cadadr cadadr)
  (extend-toplevel-env! 'caddar caddar)
  (extend-toplevel-env! 'cadddr cadddr)
  (extend-toplevel-env! 'cdaaar cdaaar)
  (extend-toplevel-env! 'cdaadr cdaadr)
  (extend-toplevel-env! 'cdadar cdadar)
  (extend-toplevel-env! 'cdaddr cdaddr)
  (extend-toplevel-env! 'cddaar cddaar)
  (extend-toplevel-env! 'cddadr cddadr)
  (extend-toplevel-env! 'cdddar cdddar)
  (extend-toplevel-env! 'cddddr cddddr)
  (extend-toplevel-env! 'null? null?)
  (extend-toplevel-env! 'list? list?)
  (extend-toplevel-env! 'list list)
  (extend-toplevel-env! 'length length)
  (extend-toplevel-env! 'append append)
  (extend-toplevel-env! 'reverse reverse)
  (extend-toplevel-env! 'list-tail list-tail)
  (extend-toplevel-env! 'list-ref list-ref)
  (extend-toplevel-env! 'memq memq)
  (extend-toplevel-env! 'memv memv)
  (extend-toplevel-env! 'member member)
  (extend-toplevel-env! 'assq assq)
  (extend-toplevel-env! 'assv assv)
  (extend-toplevel-env! 'assoc assoc)
  ;; not in R4RS:
  (extend-toplevel-env! 'last-pair last-pair)
  (extend-toplevel-env! 'remq remq)
  (extend-toplevel-env! 'remv remv)
  (extend-toplevel-env! 'remove remove)
  (extend-toplevel-env! 'append! append!)
  (extend-toplevel-env! 'reverse! reverse!)

  ;; symbols

  (extend-toplevel-env! 'symbol? symbol?)
  (extend-toplevel-env! 'symbol->string symbol->string)
  (extend-toplevel-env! 'string->symbol string->symbol)
  ;; not in R4RS:
  (extend-toplevel-env! 'symbol-hash symbol-hash)
  (extend-toplevel-env! 'gensym gensym)

  ;; numbers

  (extend-toplevel-env! 'number? number?)
  (extend-toplevel-env! 'complex? complex?)
  (extend-toplevel-env! 'real? real?)
  (extend-toplevel-env! 'rational? rational?)
  (extend-toplevel-env! 'integer? integer?)
  (extend-toplevel-env! 'exact? exact?)
  (extend-toplevel-env! 'inexact? inexact?)
  (extend-toplevel-env! '= =)
  (extend-toplevel-env! '< <)
  (extend-toplevel-env! '> >)
  (extend-toplevel-env! '<= <=)
  (extend-toplevel-env! '>= >=)
  (extend-toplevel-env! 'zero? zero?)
  (extend-toplevel-env! 'positive? positive?)
  (extend-toplevel-env! 'negative? negative?)
  (extend-toplevel-env! 'odd? odd?)
  (extend-toplevel-env! 'even? even?)
  (extend-toplevel-env! 'max max)
  (extend-toplevel-env! 'min min)
  (extend-toplevel-env! '+ +)
  (extend-toplevel-env! '* *)
  (extend-toplevel-env! '- -)
  (extend-toplevel-env! '/ /)
  (extend-toplevel-env! 'abs abs)
  (extend-toplevel-env! 'quotient quotient)
  (extend-toplevel-env! 'remainder remainder)
  (extend-toplevel-env! 'modulo modulo)
  (extend-toplevel-env! 'gcd gcd)
  (extend-toplevel-env! 'lcm lcm)
  (extend-toplevel-env! 'numerator numerator)
  (extend-toplevel-env! 'denominator denominator)
  (extend-toplevel-env! 'floor floor)
  (extend-toplevel-env! 'ceiling ceiling)
  (extend-toplevel-env! 'truncate truncate)
  (extend-toplevel-env! 'round round)
  (extend-toplevel-env! 'rationalize rationalize)
  ;; exp, log, sin, cos, tan, asin, acos, atan
  ;; (extend-toplevel-env! 'sqrt sqrt)
  (extend-toplevel-env! 'expt expt)
  (extend-toplevel-env! 'make-rectangular make-rectangular)
  (extend-toplevel-env! 'real-part real-part)
  (extend-toplevel-env! 'imag-part imag-part)
  ;; make-polar
  ;; magnitude
  ;; angle
  (extend-toplevel-env! 'exact->inexact exact->inexact)
  (extend-toplevel-env! 'inexact->exact inexact->exact)
  (extend-toplevel-env! 'number->string number->string)
  (extend-toplevel-env! 'string->number string->number)
  ;; not in R4RS:
  (extend-toplevel-env! 'random random)
  (extend-toplevel-env! 'fixnum? fixnum?)
  (extend-toplevel-env! 'bignum? bignum?)
  (extend-toplevel-env! 'ratnum? ratnum?)
  (extend-toplevel-env! 'flonum? flonum?)
  (extend-toplevel-env! 'compnum? compnum?)
  (extend-toplevel-env! 'rectnum? rectnum?)

  ;; Characters

  (extend-toplevel-env! 'char? char?)
  (extend-toplevel-env! 'char=? char=?)
  (extend-toplevel-env! 'char<? char<?)
  (extend-toplevel-env! 'char>? char>?)
  (extend-toplevel-env! 'char<=? char<=?)
  (extend-toplevel-env! 'char>=? char>=?)
  (extend-toplevel-env! 'char-ci=? char-ci=?)
  (extend-toplevel-env! 'char-ci<? char-ci<?)
  (extend-toplevel-env! 'char-ci>? char-ci>?)
  (extend-toplevel-env! 'char-ci<=? char-ci<=?)
  (extend-toplevel-env! 'char-ci>=? char-ci>=?)
  (extend-toplevel-env! 'char-alphabetic? char-alphabetic?)
  (extend-toplevel-env! 'char-numeric? char-numeric?)
  (extend-toplevel-env! 'char-whitespace? char-whitespace?)
  (extend-toplevel-env! 'char-upper-case? char-upper-case?)
  (extend-toplevel-env! 'char-lower-case? char-lower-case?)
  (extend-toplevel-env! 'char->integer char->integer)
  (extend-toplevel-env! 'integer->char integer->char)
  (extend-toplevel-env! 'char-upcase char-upcase)
  (extend-toplevel-env! 'char-downcase char-downcase)

  ;; strings

  (extend-toplevel-env! 'string? string?)
  (extend-toplevel-env! 'make-string make-string)
  (extend-toplevel-env! 'string string)
  (extend-toplevel-env! 'string-length string-length)
  (extend-toplevel-env! 'string-ref string-ref)
  (extend-toplevel-env! 'string-set! string-set!)
  (extend-toplevel-env! 'string=? string=?)
  (extend-toplevel-env! 'string<? string<?)
  (extend-toplevel-env! 'string>? string>?)
  (extend-toplevel-env! 'string<=? string<=?)
  (extend-toplevel-env! 'string>=? string>=?)
  (extend-toplevel-env! 'string-ci=? string-ci=?)
  (extend-toplevel-env! 'string-ci<? string-ci<?)
  (extend-toplevel-env! 'string-ci>? string-ci>?)
  (extend-toplevel-env! 'string-ci<=? string-ci<=?)
  (extend-toplevel-env! 'string-ci>=? string-ci>=?)
  (extend-toplevel-env! 'substring substring)
  (extend-toplevel-env! 'string-append string-append)
  (extend-toplevel-env! 'string->list string->list)
  (extend-toplevel-env! 'list->string list->string)
  (extend-toplevel-env! 'string-copy string-copy)
  (extend-toplevel-env! 'string-fill! string-fill!)
  ;; not in R4RS:
  (extend-toplevel-env! 'string-hash string-hash)
  (extend-toplevel-env! 'substring-fill! substring-fill!)

  ;; vectors

  (extend-toplevel-env! 'vector? vector?)
  (extend-toplevel-env! 'make-vector make-vector)
  (extend-toplevel-env! 'vector vector)
  (extend-toplevel-env! 'vector-length vector-length)
  (extend-toplevel-env! 'vector-ref vector-ref)
  (extend-toplevel-env! 'vector-set! vector-set!)
  (extend-toplevel-env! 'vector->list vector->list)
  (extend-toplevel-env! 'list->vector list->vector)
  (extend-toplevel-env! 'vector-fill! vector-fill!)

  ;; control features

  (extend-toplevel-env! 'procedure? procedure?)
  (extend-toplevel-env! 'apply apply)
  (extend-toplevel-env! 'map map)
  (extend-toplevel-env! 'for-each for-each)
  (extend-toplevel-env! 'force 'force)
  (extend-toplevel-env! 'call-with-current-continuation
                        call-with-current-continuation)
  ;; not in R4RS:
  (extend-toplevel-env! 'call/cc call-with-current-continuation)
  (extend-toplevel-env! 'list->procedure list->procedure)
  
  ;; i/o

  (extend-toplevel-env! 'call-with-input-file call-with-input-file)
  (extend-toplevel-env! 'call-with-output-file call-with-output-file)
  (extend-toplevel-env! 'input-port? input-port?)
  (extend-toplevel-env! 'output-port? output-port?)
  (extend-toplevel-env! 'current-input-port current-input-port)
  (extend-toplevel-env! 'current-output-port current-output-port)
  (extend-toplevel-env! 'with-input-from-file with-input-from-file)
  (extend-toplevel-env! 'with-output-to-file with-output-to-file)
  (extend-toplevel-env! 'open-input-file open-input-file)
  (extend-toplevel-env! 'open-output-file open-output-file)
  (extend-toplevel-env! 'close-input-port close-input-port)
  (extend-toplevel-env! 'close-output-port close-output-port)
  (extend-toplevel-env! 'read read)
  (extend-toplevel-env! 'read-char read-char)
  (extend-toplevel-env! 'peek-char peek-char)
  (extend-toplevel-env! 'eof-object? eof-object?)
  (extend-toplevel-env! 'char-ready? char-ready?)
  (extend-toplevel-env! 'write write)
  (extend-toplevel-env! 'display display)
  (extend-toplevel-env! 'newline newline)
  (extend-toplevel-env! 'write-char write-char)
  ;; not in R4RS:
  (extend-toplevel-env! 'port? port?)
  (extend-toplevel-env! 'flush-output-port flush-output-port)
  (extend-toplevel-env! '**eof** **eof**)
  (extend-toplevel-env! 'delete-file delete-file)
  (extend-toplevel-env! 'reset-iosystem reset-iosystem)
  (extend-toplevel-env! 'rename-file rename-file)
  (extend-toplevel-env! 'file-exists? file-exists?)
  (extend-toplevel-env! 'file-modification-time file-modification-time)

  ;; general

  (extend-toplevel-env! 'load load)
  ;; transcript-on, transcript-off (optional)

  ;; common extensions to R4RS

  (extend-toplevel-env! 'error error)
  (extend-toplevel-env! 'exit exit)
  (extend-toplevel-env! 'eval eval)

  ;; property lists

  (extend-toplevel-env! 'getprop getprop)
  (extend-toplevel-env! 'putprop putprop)
  (extend-toplevel-env! 'remprop remprop)

  ;; vector-like

  (extend-toplevel-env! 'vector-like? vector-like?)
  (extend-toplevel-env! 'vector-like-length vector-like-length)
  (extend-toplevel-env! 'vector-like-ref vector-like-ref)
  (extend-toplevel-env! 'vector-like-set! vector-like-set!)

  ;; bytevectors

  (extend-toplevel-env! 'bytevector? bytevector?)
  (extend-toplevel-env! 'bytevector-length bytevector-length)
  (extend-toplevel-env! 'make-bytevector make-bytevector)
  (extend-toplevel-env! 'bytevector-ref bytevector-ref)
  (extend-toplevel-env! 'bytevector-set! bytevector-set!)
  (extend-toplevel-env! 'bytevector-equal? bytevector-equal?)
  (extend-toplevel-env! 'bytevector-fill! bytevector-fill!)

  (extend-toplevel-env! 'bytevector-like? bytevector-like?)
  (extend-toplevel-env! 'bytevector-like-length bytevector-like-length)
  (extend-toplevel-env! 'bytevector-like-ref bytevector-like-ref)
  (extend-toplevel-env! 'bytevector-like-set! bytevector-like-set!)
  (extend-toplevel-env! 'bytevector-like-equal? bytevector-like-equal?)

  ;; Support for rewriter and for macro expansion.

  (extend-toplevel-env! 'rewrite rewrite)
  (extend-toplevel-env! '%list list)
  (extend-toplevel-env! '%list->vector list->vector)
  (extend-toplevel-env! '%cons cons)
  (extend-toplevel-env! '%append append)
  (extend-toplevel-env! '%make-promise %make-promise)

  ;; system performance

  (extend-toplevel-env! 'run-with-stats run-with-stats)
  (extend-toplevel-env! 'run-benchmark run-benchmark)
  (extend-toplevel-env! 'display-memstats display-memstats) ; mostly obsolete
  (extend-toplevel-env! 'collect collect)
  (extend-toplevel-env! 'memstats memstats)

  ;; error handling

  (extend-toplevel-env! 'print-continuation print-continuation)
  (extend-toplevel-env! 'error-continuation error-continuation)

  ;; miscellaneous extensions

  (extend-toplevel-env! 'rep-loop rep-loop)
  (extend-toplevel-env! 'issue-warnings issue-warnings)
  (extend-toplevel-env! 'load-noisily
                        (lambda () (set! load-noise-level #t)))
  (extend-toplevel-env! 'load-quietly
                        (lambda () (set! load-noise-level #f)))
  (extend-toplevel-env! 'typetag typetag)
  (extend-toplevel-env! 'typetag-set! typetag-set!)
  (extend-toplevel-env! '**newline** **newline**)
  (extend-toplevel-env! 'unspecified unspecified)
  (extend-toplevel-env! 'dumpheap dumpheap)
  (extend-toplevel-env! 'sys$tracectl sys$tracectl)
  (extend-toplevel-env! 'sys$trace sys$trace)
  (extend-toplevel-env! 'getenv getenv)

  ;; bignum debugging

  (extend-toplevel-env! 'bigdump bigdump)
  (extend-toplevel-env! 'bigdump* bigdump*)
  (extend-toplevel-env! 'big-divide-digits big-divide-digits)

  ;; flonum debugging

  (extend-toplevel-env! 'float-significand float-significand)
  (extend-toplevel-env! 'float-exponent float-exponent)

  #t)


;; A helper for the loader, needs to move to some other spot.

(define (list->procedure list)
  (let ((p (make-procedure (length list))))
    (let loop ((l list) (i 0))
      (if (null? l)
	  p
	  (begin (procedure-set! p i (car l))
		 (loop (cdr l) (+ i 1)))))))


;; To invoke collector

(define (collect . args)
  (let ((type $gc.ephemeral))
    (if (not (null? args))
	(let ((a (car args)))
	  (cond ((eq? a 'ephemeral) (set! type $gc.ephemeral))
		((eq? a 'tenuring)  (set! type $gc.tenuring))
		((eq? a 'full)      (set! type $gc.full))
		(else
		 (error "collect: bad argument "
			(car args)
			#\newline
			"use one of 'ephemeral, 'tenuring, 'full.")))))
    (sys$gc type)))

(define gensym
  (let ((n 1000))
    (lambda (x)
      (set! n (+ n 1))
      (make-symbol (string-append x (number->string n))))))

(define (atom? x) (not (pair? x)))

; This is a hack. Nobody seems to want the responsibility.

(define **newline** 13)

;; eof
