; Copyright 1991 Lightship Software, Incorporated.
;
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
