; Lib/env.sch
; Larceny library -- environments.
;
; $Id: env.sch,v 1.3 1997/08/22 21:05:14 lth Exp $
;
; An `environment' is a map from names (symbols) to locations (global cells).
;
; Environments in Larceny are hierarchical: when an environment is created,
; it can be given a parent environment, and the contents of the parent
; environment are inherited in the new environment (transitively).  
;
; Inheritance works as follows.  When a new environment is created, it 
; contains only a reference to its parent environment.  When a name is
; referenced in the new environment, and there is not a binding for that
; name in the environment but there is one in the parent, then a new
; cell is created for the name in the environment with the current value
; from the entry in the parent environment, and all subsequent references
; to the name in the environment will get the new cell.
;
; There are at least three ways to view inheritance.  
; (1) All entries in the parent are eagerly duplicated in the new 
;     environment when it is created.  Hence, all values in the new 
;     environment are definitely those of the parent at the time of 
;     creation.  The disadvantage here is the potential blowup in the 
;     size of environments, but it is a clean model.
; (2) No entries are duplicated in the new environment; cells are shared 
;     with the parent environment.  This is also clean, but defeats one
;     important use of environments, namely as namespaces.
; (3) Cells are duplicated in the child environment only on assignment or
;     when a reference to the cell (as opposed to its value) escapes, not on
;     a reference that only gets the value.  I don't like this model, since it
;     makes it possible to observe when the shadow copy is made.


($$trace "env")

; Global cells are represented as pairs.

(define make-global-cell (lambda (value name) (cons value name)))
(define global-cell-ref  (lambda (cell) (car cell)))
(define global-cell-set! (lambda (cell value) (set-car! cell value)))


; Environments are represented as vectors with a magic tag.
; FIXME: an environment should be a record (or structure).

(define (make-environment name parent . rest)
  (let ((size (if (null? rest) 10 (car rest))))
    (vector *environment-key*                ; secret key
            (make-vector size '())           ; hash table
            0                                ; count
            #t                               ; mutability flag
            name                             ; printable environment name
            parent                           ; parent environment or #f
            )))

(define (environment? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? *environment-key* (vector-ref obj 0))))

(define (environment-name env)
  (cond ((not (environment? env))
	 (error "environment-name: " env " is not an environment.")
	 #t)
	(else
	 (env.name env))))

(define (environment-variables env)
  (cond ((not env) '())
        ((not (environment? env))
         (error "environment-variables: " env " is not an environment.")
         #t)
        (else
         (let ((l '()))
           (env/enumerate-bindings* env (lambda (name value)
					  (if (not (memq name l))
					      (set! l (cons name l)))))
           l))))

(define (environment-gettable? env name)
  (cond ((not env) #f)
        ((not (environment? env))
         (error "environment-gettable: " env " is not an environment.")
         #t)
        (else
	 (let ((probe (env/lookup* env name)))
	   (and probe (not (eq? (global-cell-ref probe) (undefined))))))))

(define (environment-settable? env name)
  (cond ((not env) #f)
        ((not (environment? env))
         (error "environment-settable?: " env " is not an environment.")
         #t)
        (else 
         (env.mutable env))))

(define (environment-get env name)
  (cond ((not env) #f)
        ((not (environment? env))
         (error "environment-get: " env " is not an environment.")
         #t)
        (else
	 (let ((probe (environment-get-cell env name)))
	   (if (and probe (not (eq? (global-cell-ref probe) (undefined))))
	       (global-cell-ref probe)
	       (begin (error "environment-get: " name " is not defined.")
		      #t))))))

(define (environment-get-cell env name)
  (cond ((not env) #f)
        ((not (environment? env))
         (error "environment-get-cell: " env " is not an environment.")
         #t)
        (else
	 (let ((probe (env/lookup env name)))
	   (if (not probe)
	       (let ((probe2 (env/lookup* env name)))
		 (set! probe (env/define! env name))
		 (if probe2
		     (global-cell-set! probe (global-cell-ref probe2))
		     (global-cell-set! probe (undefined)))))
	   probe))))

(define environment-lookup-binding environment-get-cell)  ; Compatibility.

(define (environment-set! env name value)
  (cond ((not env) #f)
        ((not (environment? env))
         (error "environment-set!: " env " is not an environment.")
         #t)
        ((not (symbol? name))
         (error "environment-set!: " name " is not a valid name.")
         #t)
        ((not (env.mutable env))
         (error "environment-set!: environment is not mutable: "
                (env.name env))
         #t)
        (else
         (let ((probe (env/lookup env name)))
           (if (not probe)
               (set! probe (env/define! env name)))
           (global-cell-set! probe value)
           (unspecified)))))

(define (environment-reify env)
  (cond ((not (environment? env))
         (error "environment-reify: " env " is not an environment.")
         #t)
        (else
         env)))

; Environment operations as defined in the R5RS, somewhat extended.
;
; The variables *r4rs-environment*, *r5rs-environment*, and 
; *larceny-environment* are defined in Eval/toplevel.sch.

(define *interaction-environment* #f)

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
    ((4) 
     (make-environment "scheme-report-environment-v4"
		       *r4rs-environment*))
    ((5)
     (make-environment "scheme-report-environment-v5"
		       *r5rs-environment*))
    (else
     (error "scheme-report-environment: " version
	    " is not an accepted version number.")
     #t)))

(define (null-environment)
  (make-environment "null-environment" #f))

(define (larceny-environment)
  (make-environment "larceny-environment" *larceny-environment*))


; Internal

; Environments are represented using hash tables, keyed on symbols.
; We keep the load factor high since environment lookup rarely is on
; the truly critical path.
;
; Procedures named '...*' extract information from the parent chain as
; well as from the given object; the others do not.

(define *environment-key* (vector 'environment))
(define *environment-load* 0.75)             ; load factor at which to rehash
(define *new-size* 1.5)                      ; > 1, please.

(define (env.hashtable env) (vector-ref env 1))
(define (env.count env) (vector-ref env 2))
(define (env.mutable env) (vector-ref env 3))
(define (env.name env) (vector-ref env 4))
(define (env.parent env) (vector-ref env 5))

(define (env.hashtable! env ht) (vector-set! env 1 ht))
(define (env.count! env cnt) (vector-set! env 2 cnt))
(define (env.mutable! env flag) (vector-set! env 3 flag))
(define (env.parent! env parent) (vector-set! env 5 parent))

(define (env/enumerate-bindings env proc)
  (env/enumerate-cells env (lambda (name cell)
                             (proc name (cell-ref cell)))))

(define (env/enumerate-bindings* env proc)
  (if env
      (begin 
	(env/enumerate-cells env 
			     (lambda (name cell)
			       (proc name (cell-ref cell))))
	(env/enumerate-bindings* (env.parent env) proc))))

(define (env/enumerate-cells env proc)
  (let ((ht (env.hashtable env)))
    (do ((i (- (vector-length ht) 1) (- i 1)))
        ((< i 0))
      (do ((l (vector-ref ht i) (cdr l)))
          ((null? l))
        (proc (caar l) (cdar l))))))

(define (env/lookup env name)
  (let* ((ht (env.hashtable env))
         (l  (vector-length ht))
         (h  (remainder (symbol-hash name) l)))
    (let ((probe (assq name (vector-ref ht h))))
      (if probe
          (cdr probe)
          #f))))

(define (env/lookup* env name)
  (cond ((env/lookup env name))
        ((env.parent env)
         (env/lookup* (env.parent env) name))
        (else
         #f)))

(define (env/add-cell! env name cell)
  (let* ((ht (env.hashtable env))
         (l  (vector-length ht))
         (h  (remainder (symbol-hash name) l)))
    (env.count! env (+ (env.count env) 1))
    (vector-set! ht h (cons (cons name cell) (vector-ref ht h)))))
            
(define (env/define! env name)
  (let* ((ht    (env.hashtable env))
         (l     (vector-length ht))
         (h     (remainder (symbol-hash name) l))
         (probe (assq name (vector-ref ht h))))
    (if probe
        (cdr probe)
        (let ((cell (make-global-cell (undefined) name)))
          (env/add-cell! env name cell)
          (if (> (exact->inexact (env.count env)) (* l *environment-load*))
              (env/grow-and-rehash env))
          cell))))

(define (env/grow-and-rehash env)
  (let* ((old-size (vector-length (env.hashtable env)))
         (new-env  (make-environment "*tmp*"
                                     (env.parent env)
                                     (inexact->exact
                                       (ceiling (* old-size *new-size*))))))
    (env/enumerate-cells env (lambda (name cell)
                               (env/add-cell! new-env name cell)))
    (env.hashtable! env (env.hashtable new-env))
    (env.count! env (env.count new-env))
    env))


; Backwards compatible -- loader and reader still uses this.
;
; Install procedure which resolves global names in LOAD and EVAL.

(define global-name-resolver
  (let ((p (lambda (sym)
	     (environment-get-cell (interaction-environment) sym))))
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
