; Copyright 1991 William Clinger
;
; 14 May 1995

; Parallel assignment.

; Given a list of target registers, a list of expressions, and a
; compile-time environment, returns code to evaluate the expressions
; into the registers.
; Evaluates directly into the target registers if possible, otherwise
; pushes all arguments and then pops into the target registers.  It
; ought to degrade more gracefully.
; Another shortcoming is that it reserves all registers through the
; last target register while evaluating the arguments.

(define (cg-arguments output targets args regs frame env)
  (cond ((null? targets) '())
        (else
         (let ((para (let* ((regvars (map (lambda (reg)
                                            (cgreg-lookup-reg regs reg))
                                          targets)))
                       (parallel-assignment targets
                                            (map cons regvars targets)
                                            args))))
           (cond (para
                  (do ((targets para (cdr targets))
                       (args (cg-permute args targets para) (cdr args))
                       (regs regs
                             (let ((reg (car targets))
                                   (tos (cgreg-tos regs)))
                               (if (> reg tos)
                                   (cgreg-push regs (- reg tos))
                                   regs))))
                      ((null? targets))
                      (cg0 output (car args) (car targets) regs frame env #f)))
                 (else
                  (cg-evalargs output targets args regs frame env)))))))

; Evaluates arguments from left to right and moves them into the
; target registers.

(define (cg-evalargs output targets args regs frame env)
  (define (evalargs args regs frame temps)
    (cond ((null? args) (reverse temps))
          ((< (cgreg-tos regs) *lastreg*)
           (let* ((regs (cgreg-push regs 1))
                  (r (cgreg-tos regs)))
             (cg0 output (car args) r regs frame env #f)
             (evalargs (cdr args) regs frame (cons (cons 'r r) temps))))
          (else
           (cg0 output (car args) 'result regs frame env #f)
           (call-with-values
            (lambda () (cgframe-newtemp frame))
            (lambda (temp frame)
              (gen! output $setstk temp)
              (evalargs (cdr args) regs frame (cons (cons 's temp) temps)))))))
  (define (moveargs temps targets)
    (if (not (null? temps))
        (let* ((temp (car temps))
               (i (cdr temp)))
          (case (car temp)
            ((r) (gen! output $movereg i (car targets)))
            ((s) (gen! output $load (car targets) i))
            (else ???))
          (moveargs (cdr temps) (cdr targets)))))
  (moveargs (evalargs args regs frame '()) targets))

; Returns a permutation of the src list, permuted the same way the
; key list was permuted to obtain newkey.

(define (cg-permute src key newkey)
  (let ((alist (map cons key (iota (length key)))))
    (do ((newkey newkey (cdr newkey))
         (dest '()
               (cons (list-ref src (cdr (assq (car newkey) alist)))
                     dest)))
        ((null? newkey) (reverse dest)))))

; Given a list of register numbers,
; an association list with entries of the form (name . regnum) giving
; the variable names by which those registers are known in code,
; and a list of expressions giving new values for those registers,
; returns an ordering of the register assignments that implements a
; parallel assignment if one can be found, otherwise returns #f.

(define parallel-assignment
 (lambda (regnums alist exps)
   (if (null? regnums)
       #t
       (let ((x (toposort (dependency-graph regnums alist exps))))
         (if x (reverse x) #f)))))

(define dependency-graph
 (lambda (regnums alist exps)
   (let ((names (map car alist)))
     (do ((regnums regnums (cdr regnums))
          (exps exps (cdr exps))
          (l '() (cons (cons (car regnums)
                             (map (lambda (var) (cdr (assq var alist)))
                                  (intersection (freevariables (car exps)) names)))
                       l)))
         ((null? regnums) l)))))

; Given a nonempty graph represented as a list of the form
;     ((node1 . <list of nodes that node1 is less than or equal to>)
;      (node2 . <list of nodes that node2 is less than or equal to>)
;      ...)
; returns a topological sort of the nodes if one can be found,
; otherwise returns #f.

(define toposort
 (lambda (graph)
   (cond ((null? (cdr graph)) (list (caar graph)))
         (else (toposort2 graph '())))))

(define toposort2
 (lambda (totry tried)
   (cond ((null? totry) #f)
         ((or (null? (cdr (car totry)))
              (and (null? (cddr (car totry)))
                   (eq? (cadr (car totry))
                        (car (car totry)))))
          (if (and (null? (cdr totry)) (null? tried))
              (list (caar totry))
              (let* ((node (caar totry))
                     (x (toposort2 (map (lambda (y)
                                          (cons (car y) (remove node (cdr y))))
                                        (append (cdr totry) tried))
                                   '())))
                (if x
                    (cons node x)
                    #f))))
         (else (toposort2 (cdr totry) (cons (car totry) tried))))))

(define iota (lambda (n) (iota2 n '())))

(define iota1 (lambda (n) (cdr (iota2 (+ n 1) '()))))

(define iota2
 (lambda (n l)
   (if (zero? n)
       l
       (let ((n (- n 1)))
         (iota2 n (cons n l))))))

(define (freevariables exp)
  (freevars2 exp '()))

(define (freevars2 exp env)
  (cond ((symbol? exp)
         (if (memq exp env) '() (list exp)))
        ((not (pair? exp)) '())
        (else (let ((keyword (car exp)))
                (cond ((eq? keyword 'quote) '())
                      ((eq? keyword 'lambda)
                       (let ((env (append (make-null-terminated (cadr exp))
                                          env)))
                         (apply union
                                (map (lambda (x) (freevars2 x env))
                                     (cddr exp)))))
                      ((memq keyword '(if set! begin))
                       (apply union
                              (map (lambda (x) (freevars2 x env))
                                   (cdr exp))))
                      (else (apply union
                                   (map (lambda (x) (freevars2 x env))
                                        exp))))))))
