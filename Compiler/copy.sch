; Copyright 1998 William Clinger.
;
; $Id$
;
; 25 April 1999
;
; Given an expression in the subset of Scheme used as an intermediate language
; by Twobit, returns a newly allocated copy of the expression in which the
; local variables have been renamed and the referencing information has been
; recomputed.

(define (copy-exp exp)
  
  (define special-names (cons name:IGNORED argument-registers))
  
  (define original-names (make-hashtable symbol-hash assq))
  
  (define renaming-counter 0)
  
  (define (rename-vars vars)
    (let ((rename (make-rename-procedure)))
      (map (lambda (var)
             (cond ((memq var special-names)
                    var)
                   ((hashtable-get original-names var)
                    (rename var))
                   (else
                    (hashtable-put! original-names var #t)
                    var)))
           vars)))
  
  (define (rename-formals formals newnames)
    (cond ((null? formals) '())
          ((symbol? formals) (car newnames))
          ((memq (car formals) special-names)
           (cons (car formals)
                 (rename-formals (cdr formals)
                                 (cdr newnames))))
          (else (cons (car newnames)
                      (rename-formals (cdr formals)
                                      (cdr newnames))))))
  
  ; Environments that map symbols to arbitrary information.
  ; This data type is mutable, and uses the shallow binding technique.
  
  (define (make-env) (make-hashtable symbol-hash assq))
  
  (define (env-bind! env sym info)
    (let ((stack (hashtable-get env sym)))
      (hashtable-put! env sym (cons info stack))))
  
  (define (env-unbind! env sym)
    (let ((stack (hashtable-get env sym)))
      (hashtable-put! env sym (cdr stack))))
  
  (define (env-lookup env sym default)
    (let ((stack (hashtable-get env sym)))
      (if stack
          (car stack)
          default)))
  
  (define (env-bind-multiple! env symbols infos)
    (for-each (lambda (sym info) (env-bind! env sym info))
              symbols
              infos))
  
  (define (env-unbind-multiple! env symbols)
    (for-each (lambda (sym) (env-unbind! env sym))
              symbols))
  
  ;
  
  (define (lexical-lookup R-table name)
    (assq name R-table))
  
  (define (copy exp env notepad R-table)
    (cond ((constant? exp) exp)
          ((lambda? exp)
           (let* ((bvl (make-null-terminated (lambda.args exp)))
                  (newnames (rename-vars bvl))
                  (procnames (map def.lhs (lambda.defs exp)))
                  (newprocnames (rename-vars procnames))
                  (refinfo (map (lambda (var)
                                  (make-R-entry var '() '() '()))
                                (append newnames newprocnames)))
                  (newexp
                   (make-lambda
                    (rename-formals (lambda.args exp) newnames)
                    '()
                    refinfo
                    '()
                    '()
                    (lambda.decls exp)
                    (lambda.doc exp)
                    (lambda.body exp))))
             (env-bind-multiple! env procnames newprocnames)
             (env-bind-multiple! env bvl newnames)
             (for-each (lambda (entry)
                         (env-bind! R-table (R-entry.name entry) entry))
                       refinfo)
             (notepad-lambda-add! notepad newexp)
             (let ((newnotepad (make-notepad notepad)))
               (for-each (lambda (name rhs)
                           (lambda.defs-set!
                             newexp
                             (cons (make-definition
                                    name
                                    (copy rhs env newnotepad R-table))
                                   (lambda.defs newexp))))
                         (reverse newprocnames)
                         (map def.rhs
                              (reverse (lambda.defs exp))))
               (lambda.body-set!
                 newexp
                 (copy (lambda.body exp) env newnotepad R-table))
               (lambda.F-set! newexp (notepad-free-variables newnotepad))
               (lambda.G-set! newexp (notepad-captured-variables newnotepad)))
             (env-unbind-multiple! env procnames)
             (env-unbind-multiple! env bvl)
             (for-each (lambda (entry)
                         (env-unbind! R-table (R-entry.name entry)))
                       refinfo)
             newexp))
          ((assignment? exp)
           (let* ((oldname (assignment.lhs exp))
                  (name (env-lookup env oldname oldname))
                  (varinfo (env-lookup R-table name #f))
                  (newexp
                   (make-assignment name
                                    (copy (assignment.rhs exp) env notepad R-table))))
             (notepad-var-add! notepad name)
             (if varinfo
                 (R-entry.assignments-set!
                  varinfo
                  (cons newexp (R-entry.assignments varinfo))))
             newexp))
          ((conditional? exp)
           (make-conditional (copy (if.test exp) env notepad R-table)
                             (copy (if.then exp) env notepad R-table)
                             (copy (if.else exp) env notepad R-table)))
          ((begin? exp)
           (make-begin (map (lambda (exp) (copy exp env notepad R-table))
                            (begin.exprs exp))))
          ((variable? exp)
           (let* ((oldname (variable.name exp))
                  (name (env-lookup env oldname oldname))
                  (varinfo (env-lookup R-table name #f))
                  (newexp (make-variable name)))
             (notepad-var-add! notepad name)
             (if varinfo
                 (R-entry.references-set!
                  varinfo
                  (cons newexp (R-entry.references varinfo))))
             newexp))
          ((call? exp)
           (let ((newexp (make-call (copy (call.proc exp) env notepad R-table)
                                    (map (lambda (exp)
                                           (copy exp env notepad R-table))
                                         (call.args exp)))))
             (if (variable? (call.proc newexp))
                 (let ((varinfo
                        (env-lookup R-table
                                    (variable.name
                                     (call.proc newexp))
                                    #f)))
                   (if varinfo
                       (R-entry.calls-set!
                        varinfo
                        (cons newexp (R-entry.calls varinfo))))))
             (if (lambda? (call.proc newexp))
                 (notepad-nonescaping-add! notepad (call.proc newexp)))
             newexp))
          (else ???)))
  
  (copy exp (make-env) (make-notepad #f) (make-env)))

; For debugging.
; Given an expression, traverses the expression to confirm
; that the referencing invariants are correct.

(define (check-referencing-invariants exp . flags)
  
  (let ((check-free-variables? (memq 'free flags))
        (check-referencing? (memq 'reference flags))
        (first-violation? #t))
    
    ; env is the list of enclosing lambda expressions,
    ; beginning with the innermost.
    
    (define (check exp env)
      (cond ((constant? exp) (return exp #t))
            ((lambda? exp)
             (let ((env (cons exp env)))
               (return exp
                       (and (every? (lambda (exp)
                                      (check exp env))
                                    (map def.rhs (lambda.defs exp)))
                            (check (lambda.body exp) env)
                            (if (and check-free-variables?
                                     (not (null? env)))
                                 (subset? (difference
                                           (lambda.F exp)
                                           (make-null-terminated
                                            (lambda.args exp)))
                                          (lambda.F (car env)))
                                #t)
                            (if check-referencing?
                                (let ((env (cons exp env))
                                      (R (lambda.R exp)))
                                  (every? (lambda (formal)
                                            (or (ignored? formal)
                                                (R-entry R formal)))
                                          (make-null-terminated
                                           (lambda.args exp))))
                                #t)))))
            ((variable? exp)
             (return exp
                     (and (if (and check-free-variables?
                                   (not (null? env)))
                              (memq (variable.name exp)
                                    (lambda.F (car env)))
                              #t)
                          (if check-referencing?
                              (let ((Rinfo (lookup env (variable.name exp))))
                                (if Rinfo
                                    (memq exp (R-entry.references Rinfo))
                                    #t))
                              #t))))
            ((assignment? exp)
             (return exp
                     (and (check (assignment.rhs exp) env)
                          (if (and check-free-variables?
                                   (not (null? env)))
                              (memq (assignment.lhs exp)
                                    (lambda.F (car env)))
                              #t)
                          (if check-referencing?
                              (let ((Rinfo (lookup env (assignment.lhs exp))))
                                (if Rinfo
                                    (memq exp (R-entry.assignments Rinfo))
                                    #t))
                              #t))))
            ((conditional? exp)
             (return exp
                     (and (check (if.test exp) env)
                          (check (if.then exp) env)
                          (check (if.else exp) env))))
            ((begin? exp)
             (return exp
                     (every? (lambda (exp) (check exp env))
                             (begin.exprs exp))))
            ((call? exp)
             (return exp
                     (and (check (call.proc exp) env)
                          (every? (lambda (exp) (check exp env))
                                  (call.args exp))
                          (if (and check-referencing?
                                   (variable? (call.proc exp)))
                              (let ((Rinfo (lookup env
                                                   (variable.name 
                                                    (call.proc exp)))))
                                (if Rinfo
                                    (memq exp (R-entry.calls Rinfo))
                                    #t))
                              #t))))
            (else ???)))
    
    (define (return exp flag)
      (cond (flag
             #t)
            (first-violation?
             (set! first-violation? #f)
             (display "Violation of referencing invariants")
             (newline)
             (pretty-print (make-readable exp))
             #f)
            (else (pretty-print (make-readable exp))
                  #f)))
    
    (define (lookup env I)
      (if (null? env)
          #f
          (let ((Rinfo (R-entry (lambda.R (car env)) I)))
            (or Rinfo
                (lookup (cdr env) I)))))
    
    (if (null? flags)
        (begin (set! check-free-variables? #t)
               (set! check-referencing? #t)))
    
    (check exp '())))


; Calculating the free variable information for an expression
; as output by pass 2.  This should be faster than computing both
; the free variables and the referencing information.

(define (compute-free-variables! exp)
  
  (define empty-set (make-set '()))
  
  (define (singleton x) (list x))
  
  (define (union2 x y) (union x y))
  (define (union3 x y z) (union x y z))
  
  (define (set->list set) set)
  
  (define (free exp)
    (cond ((constant? exp) empty-set)
          ((lambda? exp)
           (let* ((defs (lambda.defs exp))
                  (formals (make-set
                            (make-null-terminated (lambda.args exp))))
                  (defined (make-set (map def.lhs defs)))
                  (Fdefs
                   (apply-union
                    (map (lambda (def)
                           (free (def.rhs def)))
                         defs)))
                  (Fbody (free (lambda.body exp)))
                  (F (union2 Fdefs Fbody)))
             (lambda.F-set! exp (set->list F))
             (lambda.G-set! exp (set->list F))
             (difference F (union2 formals defined))))
          ((assignment? exp)
           (union2 (make-set (list (assignment.lhs exp)))
                   (free (assignment.rhs exp))))
          ((conditional? exp)
           (union3 (free (if.test exp))
                   (free (if.then exp))
                   (free (if.else exp))))
          ((begin? exp)
           (apply-union
            (map (lambda (exp) (free exp))
                 (begin.exprs exp))))
          ((variable? exp)
           (singleton (variable.name exp)))
          ((call? exp)
           (union2 (free (call.proc exp))
                   (apply-union
                    (map (lambda (exp) (free exp))
                         (call.args exp)))))
          (else ???)))
  
  (free exp))

; As above, but representing sets as hashtrees.
; This is commented out because it is much slower than the implementation
; above.  Because the set of free variables is represented as a list
; within a lambda expression, this implementation must convert the
; representation for every lambda expression, which is quite expensive
; for A-normal form.

(begin
'
(define (compute-free-variables! exp)
  
  (define empty-set (make-hashtree symbol-hash assq))
  
  (define (singleton x)
    (hashtree-put empty-set x #t))
  
  (define (make-set values)
    (if (null? values)
        empty-set
        (hashtree-put (make-set (cdr values))
                      (car values)
                      #t)))
  
  (define (union2 x y)
    (hashtree-for-each (lambda (key val)
                         (set! x (hashtree-put x key #t)))
                       y)
    x)
  
  (define (union3 x y z)
    (union2 (union2 x y) z))
  
  (define (apply-union sets)
    (cond ((null? sets)
           (make-set '()))
          ((null? (cdr sets))
           (car sets))
          (else
           (union2 (car sets)
                   (apply-union (cdr sets))))))
  
  (define (difference x y)
    (hashtree-for-each (lambda (key val)
                         (set! x (hashtree-remove x key)))
                       y)
    x)
  
  (define (set->list set)
    (hashtree-map (lambda (sym val) sym) set))
  
  (define (free exp)
    (cond ((constant? exp) empty-set)
          ((lambda? exp)
           (let* ((defs (lambda.defs exp))
                  (formals (make-set
                            (make-null-terminated (lambda.args exp))))
                  (defined (make-set (map def.lhs defs)))
                  (Fdefs
                   (apply-union
                    (map (lambda (def)
                           (free (def.rhs def)))
                         defs)))
                  (Fbody (free (lambda.body exp)))
                  (F (union2 Fdefs Fbody)))
             (lambda.F-set! exp (set->list F))
             (lambda.G-set! exp (set->list F))
             (difference F (union2 formals defined))))
          ((assignment? exp)
           (union2 (make-set (list (assignment.lhs exp)))
                   (free (assignment.rhs exp))))
          ((conditional? exp)
           (union3 (free (if.test exp))
                   (free (if.then exp))
                   (free (if.else exp))))
          ((begin? exp)
           (apply-union
            (map (lambda (exp) (free exp))
                 (begin.exprs exp))))
          ((variable? exp)
           (singleton (variable.name exp)))
          ((call? exp)
           (union2 (free (call.proc exp))
                   (apply-union
                    (map (lambda (exp) (free exp))
                         (call.args exp)))))
          (else ???)))
  
  (hashtree-map (lambda (sym val) sym)
                (free exp)))
#t)