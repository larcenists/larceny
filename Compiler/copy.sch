; Copyright 1998 William Clinger.
;
; $Id$
;
; Given an expression in the subset of Scheme used as an intermediate language
; by Twobit, returns a newly allocated copy of the expression in which the
; local variables have been renamed and the referencing information has been
; recomputed.

(define (copy-exp exp)
  
  (define original-names '())
  
  (define renaming-counter 0)
  
  (define (rename vars)
    (set! renaming-counter (+ renaming-counter 1))
    (let ((s (string-append "_" (number->string renaming-counter))))
      (map (lambda (var)
             (if (memq var original-names)
                 (string->symbol
                  (string-append renaming-prefix
                                 (symbol->string var) s))
                 (begin (set! original-names (cons var original-names))
                        var)))
           vars)))
  
  (define (rename-formals formals newnames)
    (cond ((null? formals) '())
          ((symbol? formals) (car newnames))
          (else (cons (car newnames)
                      (rename-formals (cdr formals)
                                      (cdr newnames))))))
  
  (define (copy exp env notepad R-table)
    (cond ((constant? exp) exp)
          ((lambda? exp)
           (let* ((bvl (make-null-terminated (lambda.args exp)))
                  (newnames (rename bvl))
                  (procnames (map def.lhs (lambda.defs exp)))
                  (newprocnames (rename procnames))
                  (refinfo (map (lambda (var)
                                  (make-R-entry var '() '() '()))
                                (append newnames newprocnames)))
                  (R-table (append refinfo R-table))
                  (newenv (append (map cons procnames newprocnames)
                                  (append (map cons bvl newnames) env)))
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
             (notepad-lambda-add! notepad newexp)
             (let ((newnotepad (make-notepad notepad)))
               (for-each (lambda (name rhs)
                           (lambda.defs-set!
                             newexp
                             (cons (make-definition
                                    name
                                    (copy rhs newenv newnotepad R-table))
                                   (lambda.defs newexp))))
                         (reverse newprocnames)
                         (map def.rhs
                              (reverse (lambda.defs exp))))
               (lambda.body-set!
                 newexp
                 (copy (lambda.body exp) newenv newnotepad R-table))
               (lambda.F-set! newexp (notepad-free-variables newnotepad))
               (lambda.G-set! newexp (notepad-captured-variables newnotepad)))
             newexp))
          ((assignment? exp)
           (let* ((name (let* ((I (assignment.lhs exp))
                               (x (assq I env)))
                          (if x (cdr x) I)))
                  (varinfo (lexical-lookup R-table name))
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
           (let* ((name (let* ((I (assignment.lhs exp))
                               (x (assq I env)))
                          (if x (cdr x) I)))
                  (varinfo (lexical-lookup R-table name))
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
                        (lexical-lookup R-table
                                        (variable.name
                                         (call.proc newexp)))))
                   (if varinfo
                       (R-entry.calls-set!
                        varinfo
                        (cons newexp (R-entry.calls varinfo))))))
             (if (lambda? (call.proc newexp))
                 (notepad-nonescaping-add! notepad (call.proc newexp)))
             newexp))
          (else ???)))
  (define (lexical-lookup R-table name)
    (assq name R-table))
  (copy exp '() (make-notepad #f) '()))

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


