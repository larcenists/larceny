; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- environments.

($$trace "env")

; R5RS environment operations, somewhat extended.

(define *null-environment*)
(define *r4rs-environment*)
(define *r5rs-environment*)
(define *interaction-environment*)

(define (install-environments! null r4rs r5rs larceny)
  (set! *null-environment* null)
  (set! *r4rs-environment* r4rs)
  (set! *r5rs-environment* r5rs)
  (set! *interaction-environment* larceny)
  (unspecified))

(define (interaction-environment . rest)
  (cond ((null? rest)
	 *interaction-environment*)
	((and (null? (cdr rest)))
	 (if (and (environment? (car rest))
		  (env.mutable (car rest)))
	     (set! *interaction-environment* (car rest))
	     (error "interaction-environment: " (car rest) 
		    " is not a mutable environment."))
	 (unspecified))
	(else
	 (error "interaction-environment: too many arguments.")
	 #t)))

(define (scheme-report-environment version)
  (case version
    ((4)  *r4rs-environment*)
    ((5)  *r5rs-environment*)
    (else (error "scheme-report-environment: " version
		 " is not an accepted version number.")
	  #t)))

(define (null-environment version)
  (case version
    ((4 5) *null-environment*)
    (else  (error "null-environment: " version 
		  " is not an accepted version number.")
	   #t)))


; Global cells are represented as pairs, for now.  The compiler
; knows this, don't change it willy-nilly.

(define make-global-cell (lambda (value name) (cons value name)))
(define global-cell-ref  (lambda (cell) (car cell)))
(define global-cell-set! (lambda (cell value) (set-car! cell value)))


; Environment operations
;
; The rule is that an identifier has one denotation: it's either a
; variable or a macro, and it can transition from one to the other
; and back.  By default it is an identifier.
;
; The problem is that the macro expander can remove and add macros
; behind the back of this interface, so we must check the macro env
; every time.

(define *environment-key* (vector 'environment))

(define (make-environment name)
  (let ((env (make-structure 5)))
    (vector-like-set! env 0 *environment-key*)
    (vector-like-set! env 1 (make-hashtable symbol-hash assq))
    (vector-like-set! env 2 #t)
    (vector-like-set! env 3 name)
    (vector-like-set! env 4 (make-minimal-syntactic-environment))
    env))

(define (env.hashtable env) (vector-like-ref env 1))
(define (env.mutable env) (vector-like-ref env 2))
(define (env.name env) (vector-like-ref env 3))
(define (env.syntaxenv env) (vector-like-ref env 4))

(define (env.mutable! env flag) (vector-like-set! env 2 flag))

(define (environment? obj)
  (and (structure? obj)
       (> (vector-like-length obj) 0)
       (eq? *environment-key* (vector-like-ref obj 0))))

(define (environment-name env)
  (check-environment env 'environment-name)
  (env.name env))

(define (environment-variables env)
  (check-environment env 'environment-variables)
  (let ((macros (environment-macros env))
        (variables '()))
    (hashtable-for-each (lambda (id cell) 
                          (if (not (memq id macros))
                              (set! variables (cons id variables))))
                        (env.hashtable env))
    variables))

(define (environment-variable? env name)
  (check-environment env 'environment-variable?)
  (let ((probe1 (hashtable-get (env.hashtable env) name))
        (probe2 (environment-macro? env name)))
    (and (not probe2)
         probe1
         (not (eq? (global-cell-ref probe1) (undefined))))))
  
(define (environment-get env name)
  (check-environment env 'environment-get)
  (if (not (environment-macro? env name))
      (let ((probe (environment-get-cell env name)))
        (if (not (eq? (global-cell-ref probe) (undefined)))
            (global-cell-ref probe)
            (begin (error "environment-get: not defined: " name)
                   #t)))
      (begin (error "environment-get: denotes a macro: " name)
             #t)))

(define (environment-get-cell env name)
  (check-environment env 'environment-get-cell)
  (if (not (environment-macro? env name))
      (or (hashtable-get (env.hashtable env) name)
          (let ((cell (make-global-cell (undefined) name)))
            (hashtable-put! (env.hashtable env) name cell)
            cell))
      (begin 
        (error "environment-get-cell: denotes a macro: " name)
        #t)))

(define (environment-set! env name value)
  (check-environment env 'environment-set!)
  (cond ((not (env.mutable env))
         (error "environment-set!: environment is not mutable: "
                (env.name env))
         #t)
        ((environment-macro? env name)
         (syntactic-environment-remove! (environment-syntax-environment env)
                                        name)
         (environment-set! env name value))
        (else
         (let ((cell (environment-get-cell env name)))
           (global-cell-set! cell value)
           (unspecified)))))

(define (environment-syntax-environment env)
  (check-environment env 'environment-syntax-environment)
  (env.syntaxenv env))

(define (environment-copy env . rest)
  (check-environment env 'environment-copy)
  (let* ((name      (if (null? rest) (environment-name env) (car rest)))
         (new       (make-environment name))
         (variables (environment-variables env))
         (macros    (environment-macros env)))
    (do ((vs variables (cdr vs)))
        ((null? vs))
      (if (environment-variable? env (car vs))
          (environment-set! new (car vs) (environment-get env (car vs)))))
    (do ((ms macros (cdr ms)))
        ((null? ms))
      (environment-set-macro! new (car ms) 
                              (environment-get-macro env (car ms))))
    new))

(define (environment-macros env)
  (check-environment env 'environment-macros)
  (syntactic-environment-names (environment-syntax-environment env)))

(define (environment-get-macro env id)
  (check-environment env 'environment-get-macro)
  (syntactic-environment-get (environment-syntax-environment env) id))

(define (environment-set-macro! env id macro)
  (check-environment env 'environment-set-macro!)
  (hashtable-remove! (env.hashtable env) id)
  (syntactic-environment-set! (environment-syntax-environment env) id macro))

(define (environment-macro? env id)
  (check-environment env 'environment-macro?)
  (not (not (syntactic-environment-get (environment-syntax-environment env) 
                                       id))))

(define (check-environment env tag)
  (if (not (environment? env))
      (error tag ": not an environment: " env)))

; LOAD still uses this (though READ).
;
; The initial environment is undefined, to avoid capturing a lot of
; global bindings if the reader is included in a dumped heap that
; does not use LOAD.

(define global-name-resolver
  (let ((p (lambda (sym)
             (error "GLOBAL-NAME-RESOLVER: not installed."))))
    (lambda rest
      (cond ((null? rest) p)
	    ((and (null? (cdr rest))
		  (procedure? (car rest)))
	     (let ((old p))
	       (set! p (car rest))
	       old))
	    (else
	     (error "global-name-resolver: Wrong number of arguments: "
		    rest)
	     #t)))))

     
; eof
