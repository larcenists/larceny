; Miscellaneous trivial patches.

; FIXME:  What about lambda.doc ?

; Modifies sparc.imp.sch

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 8)

; Variable names that indicate register targets.

(define *regnames*
  (do ((alist '() (cons (cons (string->symbol
                               (string-append ".REG" (number->string r)))
                              r)
                        alist))
       (r (- *nhwregs* 1) (- r 1)))
      ((<= r 0)
       alist)))

; Improves the code generated for calls to LIST, MAP, FOR-EACH.

(define-inline 'list
  (lambda (exp env)
    (cond ((null? (cdr exp)) (make-constant '()))
          ((null? (cddr exp))
           (make-call (make-variable 'cons)
                      (list (m-scan (cadr exp) env)
                            (make-constant '()))))
          (else (make-call (make-variable 'list)
                           (map (lambda (x)
                                  (m-scan x env))
                                (cdr exp)))))))

(define-inline 'map
  (lambda (exp env)
    (if (and (list? exp)
             (>= (length exp) 3)
             (let ((f (cadr exp)))
               (or (symbol? f)
                   (and (pair? f)
                        (eq? (car f) 'lambda)))))
        (let ((result (gensym "mapT_"))
              (vars (map (lambda (x) (gensym "mapT_"))
                         (cddr exp)))
              (f (cadr exp))
              (args (cddr exp)))
          (m-scan `(do ((,result '()
                                 (cons (,f ,@(map (lambda (v)
                                                    `(car ,v))
                                                  vars))
                                       ,result))
                        ,@(map (lambda (v exp)
                                 `(,v ,exp (cdr ,v)))
                               vars
                               args))
                       ((or ,@(map (lambda (v)
                                     `(null? ,v))
                                   vars))
                        (reverse ,result)))
                  env))
        (make-call (make-variable 'map)
                   (map (lambda (x)
                          (m-scan x env))
                        (cdr exp))))))

(define-inline 'for-each
  (lambda (exp env)
    (if (and (list? exp)
             (>= (length exp) 3)
             (let ((f (cadr exp)))
               (or (symbol? f)
                   (and (pair? f)
                        (eq? (car f) 'lambda)))))
        (let ((vars (map (lambda (x) (gensym "for-eachT_"))
                         (cddr exp)))
              (f (cadr exp))
              (args (cddr exp)))
          (m-scan `(do (,@(map (lambda (v exp)
                                 `(,v ,exp (cdr ,v)))
                               vars
                               args))
                       ((or ,@(map (lambda (v)
                                     `(null? ,v))
                                   vars)))
                       (,f ,@(map (lambda (v)
                                    `(car ,v))
                                  vars)))
                  env))
        (make-call (make-variable 'for-each)
                   (map (lambda (x)
                          (m-scan x env))
                        (cdr exp))))))



; More miscellaneous.

(define $car:pair 'car)
(define $cdr:pair 'cdr)

; Modifies Asm/Sparc/pass5p2.sch
; Cosmetic patch to listify? output.

'
(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop.2arg! as
		       (operand1 instruction)
		       $r.argreg2)))
