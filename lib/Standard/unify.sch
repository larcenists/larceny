; Copyright 1999 Lars T Hansen
;
; Permission to use this code for any purpose whatsoever is hereby
; granted, provided that the above copyright notice and this legend
; are preserved in any work using this code.
;
; 2000-05-26 / lth
;
; Usage
;  (make-unifier variable? variable=? perform-occurs-check?) =>
;        unifier-procedure, undefined-predicate?
;
;  (variable? obj)                       => boolean
;  (variable=? var1 var2)                => #f
;  (variable=? var var)                  => #t
;  perform-occurs-check?                 boolean
;
;  (unifier-procedure arg1 arg2)         => boolean or instantiation
;  (undefined-predicate? datum)          => boolean
;
; Description
;  Structures containing constants, pairs, vectors, and variables are 
;  unified.
;
;  If the unification succeeds, an assoc list that maps variables to 
;  their MGU values is returned.  If there are variables that are unified
;  with each other but not with a value, they are represented in the assoc
;  list with a value that is a pair where the car is a value for which the
;  predicate undefined-predicate? returns #t, and where the cdr is a
;  nonnegative exact integer that is unique to that group of unified
;  variables.
;
;  If perform-occurs-check? is #t then unification between a variable 
;  and a structure containing that variable will cause the unification 
;  to fail.
;
;  If the unification fails for any reason, #f is returned.
;
; Syntax
;  datum      --> constant
;               | variable
;               | structure
;  constant   --> (quote X)
;               | procedure
;               | null
;               | string 
;               | number 
;               | character
;               | boolean
;               | symbol
;               | <other data that do not look like structures>
;  structure  --> ( datum datum ... )
;               | #( datum ... )
;  variable   --> <implementation-defined>
;
;  Constants are compared using equal?, after first removing any quoting.
;  Variables need not be disjoint from other data: if a datum is 
;  interpretable as a variable, then that interpretation takes precedence.
;
; Wishlist
;  We can parameterize make-unifier over the representation of constants
;  also by allowing constant? and constant=? procedure to be passed in.
;
;  Perhaps provide a procedure _substitute_ (or even _substitute!_) that
;  will take a datum and an instantiation and that will substitute all
;  variable bindings in the datum with their values from the
;  instantiation.  (Var-var unification must be dealt with by leaving a
;  variable in the structure, sharing a name.)  Perhaps a make-substituter
;  procedure is appropriate.

(define (make-unifier variable? variable=? perform-occurs-check?)

  (define (unify a b)
    (let ((env   (empty-env))
          (store (empty-store)))
      (and (unify0 a b env store)
           (map-bindings (lambda (name loc)
                           (let ((v (fetch store loc)))
                             (if (undefined? v)
                                 (cons name (cons (make-undefined) loc))
                                 (cons name v))))
                         env))))

  (define (unify0 a b env store)
    (cond ((variable? a)
           (cond ((variable? b) (unify-variables a b env store))
                 ((constant? b) (unify-const-and-var b a env store))
                 (else  
                  ; (structure? b)
                  (unify-struct-and-var b a env store))))
          ((constant? a) 
           (cond ((constant? b) (constant=? a b))
                 ((variable? b) (unify-const-and-var a b env store))
                 (else #f)))
          (else  
           ; (structure? a)
           (cond ((variable? b) (unify-struct-and-var a b env store))
                 ((structure? b) (unify-structures a b env store))
                 (else #f)))))

  (define (unify-structures a b env store)
    (cond ((and (pair? a) (pair? b))
           (and (unify0 (car a) (car b) env store)
                (unify0 (cdr a) (cdr b) env store)))
          ((and (vector? a) 
                (vector? b) 
                (= (vector-length a) (vector-length b)))
           (let loop ((i (- (vector-length a) 1)))
             (cond ((< i 0) #t)
                   ((= i 0) 
                    (unify0 (vector-ref a 0) (vector-ref b 0) env store))
                   (else 
                    (and (unify0 (vector-ref a i) (vector-ref b i) env store)
                         (loop (- i 1)))))))
          (else #f)))

  (define (unify-variables a b env store)
    (let* ((loc-a (find-var a env store))
           (loc-b (find-var b env store))
           (val-a (fetch store loc-a))
           (val-b (fetch store loc-b)))
      (cond ((and (undefined? val-a) (undefined? val-b))
             (for-each (lambda (var)
                         (bind! env var loc-a))
                       (undefined-tags val-b))
             (let ((tags (append (undefined-tags val-a)
                                 (undefined-tags val-b))))
               (undefined-tags! val-a tags)
               (undefined-tags! val-b tags)
               #t))
            ((undefined? val-a)
             (unify0 a val-b env store))
            ((undefined? val-b)
             (unify0 val-a b env store))
            (else
             (unify0 val-a val-b env store)))))

  (define (unify-struct-and-var struct var env store)
    (if (and perform-occurs-check? 
             (occurs? var struct))
        #f
        (let* ((loc (find-var var env store))
               (val (fetch store loc)))
          (if (undefined? val)
              (begin (store! store loc struct)
                     (undefined-tags! val '())
                     #t)
              (unify0 struct val env store)))))

  (define (unify-const-and-var const var env store)
    (let* ((loc   (find-var var env store))
           (const (constant-value const))
           (val (fetch store loc)))
      (if (undefined? val)
          (begin (store! store loc const)
                 (undefined-tags! val '())
                 #t)
          (equal? val const))))

  (define (occurs? var struct)
    (cond ((variable? struct) 
           (variable=? var struct))
          ((structure? struct)
           (cond ((pair? struct) 
                  (or (occurs? var (car struct))
                      (occurs? var (cdr struct))))
                 ((vector? struct)
                  (let loop ((i 0))
                    (cond ((= i (vector-length struct)) #f)
                          ((occurs? var (vector-ref struct i)))
                          (else (loop (+ i 1))))))
                 (else ???)))
          (else #f)))

  (define undefined-tag (vector 'undefined))     ; unique for every unifier

  (define (make-undefined . variables)
    (cons undefined-tag variables))

  (define (undefined-tags x) (cdr x))

  (define (undefined-tags! x t) (set-cdr! x t))

  (define (undefined? x) 
    (and (pair? x)
         (eq? (car x) undefined-tag)))

  (define (find-var v env store)
    (or (lookup env v)
        (let ((loc (new-loc! store)))
          (bind! env v loc)
          (store! store loc (make-undefined v))
          loc)))

  ; Classification.  Constants, structures, and variables must be disjoint.

  (define (constant? x)
    (and (not (variable? x))
         (not (structure? x))))

  (define (structure? x)
    (and (not (variable? x))
         (or (vector? x)
             (and (pair? x)
                  (not (eq? (car x) 'quote))))))

  (define (constant=? a b)
    (equal? (constant-value a) (constant-value b)))

  (define (constant-value x)
    (if (and (pair? x) (eq? (car x) 'quote))
        (cadr x)
        x))

  ; Environments are assoc lists.  They could be hash tables.

  (define (empty-env) (list '()))

  (define (bind! env name loc)
    (let ((probe (lookup-binding name (car env))))
      (if probe
          (set-cdr! probe loc)
          (set-car! env (cons (cons name loc) (car env))))))

  (define (lookup env name)
    (let ((probe (lookup-binding name (car env))))
      (if probe
          (cdr probe)
          #f)))

  (define (lookup-binding v e)
    (cond ((null? e) #f)
          ((variable=? (caar e) v) (car e))
          (else (lookup-binding v (cdr e)))))

  (define (map-bindings proc env)
    (map (lambda (b) (proc (car b) (cdr b))) (car env)))

  ; Stores are assoc lists.  They could be vectors.

  (define (empty-store) (list '() 0))

  (define (new-loc! store)
    (let ((loc (cadr store)))
      (set-car! (cdr store) (+ loc 1))
      loc))

  (define (store! store loc val)
    (let ((probe (assv loc (car store))))
      (if probe 
          (set-cdr! probe val)
          (set-car! store (cons (cons loc val) (car store))))))

  (define (fetch store loc)
    (cdr (assv loc (car store))))

  (values (lambda (a b) (unify a b))
          (lambda (x) (undefined? x))))

; eof
