; OPERATIONS

(define $operation-table$ (vector '() '() '() '() '() '()))
(define (define-operation argc op handler)
  (vector-set! $operation-table$ argc
               (cons (cons op handler) (vector-ref $operation-table$ argc))))
(define (define-imm2-operation op handler)
  (vector-set! $operation-table$ 4
               (cons (cons op handler) (vector-ref $operation-table$ 4))))
(define (define-peephole-operation op handler)
  (vector-set! $operation-table$ 5
               (cons (cons op handler) (vector-ref $operation-table$ 5))))
(define (lookup-operation argc op)
  (assq op (vector-ref $operation-table$ argc)))
(define (lookup-imm2-operation op)
  (assq op (vector-ref $operation-table$ 4)))
(define (lookup-peephole-operation op)
  (assq op (vector-ref $operation-table$ 5)))

;; il:call-opX : symbol number boolean -> ilpackage
(define (il:call-opX opcode argc can-call-scheme?)
  (if (codegen-option 'new-operations)
      (il:call '(virtual instance)
               (if can-call-scheme? iltype-void iltype-schemeobject)
               il-schemeobject
               (twobit-format #f "op_~a" (csharp-op-name opcode))
               (let loop ((argc (- argc 1)))
                 (if (positive? argc)
                     (cons iltype-schemeobject (loop (- argc 1)))
                     '())))
      (if can-call-scheme?
          (il:with-saved-result
           (il:call '()
                    iltype-void
                    il-ops-special
                    (twobit-format #f "op~a_~a" argc (csharp-op-name opcode))
                    (let loop ((argc argc))
                      (if (zero? argc)
                          '()
                          (cons iltype-schemeobject (loop (- argc 1)))))))
          (il:call '()
                   iltype-schemeobject
                   il-ops
                   (twobit-format #f "op~a_~a" argc (csharp-op-name opcode))
                   (let loop ((argc argc))
                     (if (zero? argc)
                         '()
                         (cons iltype-schemeobject (loop (- argc 1)))))))))



;; opX : assembler (opcode -> boolean) symbol instruction thunk -> void
;; If there is a special implementation defined in the operations-table,
;; use that. Otherwise, call the generic implementation.
(define (opX argc immediate?)
  (lambda (instruction as)
    (list-instruction/line (twobit-format #f "op~a" argc) instruction as)
    (cond ((and (not immediate?) (lookup-operation argc (operand1 instruction)))
           => (lambda (h) (apply (cdr h) as (cddr instruction))))
          ((and immediate? (lookup-imm2-operation (operand1 instruction)))
           => (lambda (h) (apply (cdr h) as (cddr instruction))))
          (else (opX/rt argc immediate? instruction as)))))
(define (opX/rt argc immediate? instruction as)
  (let ((load-arguments-code
         (list
          (il:load-register 'result)
          (case argc
            ((1) '())
            ((2)
             (if immediate?
                 (il:load-constant (operand2 instruction))
                 (il:load-register (operand2 instruction))))
            ((3) (list (il:load-register (operand2 instruction))
                       (il:load-register (operand3 instruction))))
            (else (error "opX: argc = " argc))))))
    (if (opX-implicit-continuation? (operand1 instruction))
        (let ((numeric (allocate-label as)))
          (emit as
                (il:comment "operation ~s" (operand1 instruction))
                (il:comment "  implicit continuation LABEL ~s; JUMP INDEX ~s"
                            numeric (intern-label as numeric))
                (rep:set-implicit-continuation numeric)
                load-arguments-code
                (il:call-opX (operand1 instruction) argc #t)
                (rep:reset-implicit-continuation)
                (il:label numeric)))
        (emit as
              load-arguments-code
              (il:set-register 'result
                               (il:call-opX (operand1 instruction) argc #f))))))

(define opX-implicit-continuation? op1-implicit-continuation?)

(define (peephole-operation)
  (lambda (instruction as)
    (cond ((lookup-peephole-operation (operand1 instruction))
           => (lambda (h) (apply (cdr h) as (cddr instruction))))
          (else (error "Operation not supported: " instruction)))))

;; Instructions

(define-instruction $op1
  (opX 1 #f))

(define-instruction $op2
  (opX 2 #f))

(define-instruction $op2imm
  (opX 2 #t))

(define-instruction $op3
  (opX 3 #f))

(define-instruction $op1/branchf
  (peephole-operation))

(define-instruction $reg/op1/check
  (peephole-operation))

(define-instruction $reg/op2/check
  (peephole-operation))

(define-instruction $reg/op2imm/check
  (peephole-operation))

;; -----------------
;; Operations
;; -----------------

(define-operation 1 'creg
  (lambda (as)
    (emit as
          (il:call '() iltype-schemeobject il-cont "getCC" '())
          (il:set-register/pop 'result))))
(define-operation 1 'creg-set!
  (lambda (as)
    (emit as
          (il:load-register 'result)
          (il:call '() iltype-void il-cont "setCC" (list iltype-schemeobject))
          (il:set-register 'result (il:load-constant (unspecified))))))
(define-operation 1 'break
  (lambda (as)
    (emit as
          (il:fault $ex.breakpoint))))
(define-operation 1 'gc-counter
  (lambda (as)
    (emit as
          (il:fault $ex.unsupported))))
(define-operation 1 'not
  (lambda (as)
    (emit as
          (il:load-register 'result)
          (il:load-constant #f)
          (il 'ceq)
          (rep:make-boolean)
          (il:set-register/pop 'result))))

;; Predicates

(define-operation 2 'eq?
  (lambda (as reg2)
    (emit as
          (il:load-register 'result)
          (il:load-register reg2)
          (il 'ceq)
          (rep:make-boolean)
          (il:set-register/pop 'result))))

;;(define (define-type-predicate op type)
;;  (define-operation 1 op
;;    (lambda (as)
;;      (let ((true-label (allocate-label as))
;;            (done-label (allocate-label as)))
;;        (emit as
;;              (il:load-register 'result)
;;              (il 'isinst type)
;;              (il:branch-s 'brtrue true-label)
;;              (il:set-register 'result (il:load-constant #f))
;;              (il:branch-s 'br done-label)
;;              (il:label true-label)
;;              (il:set-register 'result (il:load-constant #t))
;;              (il:label done-label))))))
(define (define-predicate op method)
  (define-operation 1 op
    (lambda (as)
      (emit as
            (il:load-register 'result)
            (il:call '(instance virtual) iltype-bool il-schemeobject method '())
            (rep:make-boolean)
            (il:set-register/pop 'result)))))

;(define-predicate 'complex? "isComplex")
;(define-predicate 'real? "isReal")
;(define-predicate 'rational? "isRational")
;(define-predicate 'integer? "isInteger")
;;(define-type-predicate 'fixnum? iltype-fixnum)
;(define-predicate 'fixnum? "isFixnum")
;;(define-type-predicate 'pair? iltype-schemepair)
;(define-predicate 'pair? "isPair")
;(define-predicate 'flonum? "isFlonum")
;(define-predicate 'compnum? "isCompnum")
;(define-predicate 'symbol? "isSymbol")
;(define-predicate 'vector? "isVector")
;;(define-type-predicate 'vector-like? iltype-svl)
;(define-predicate 'vector-like? "isVectorLike")
;(define-predicate 'bytevector? "isByteVector")
;;(define-type-predicate 'bytevector-like? iltype-sbytevl)
;(define-predicate 'bytevector-like? "isByteVectorLike")
;(define-predicate 'port? "isPort")
;(define-predicate 'structure? "isStructure")
;(define-predicate 'string? "isString")
;;(define-type-predicate 'char? iltype-schemechar)
;(define-predicate 'char? "isChar")
;;(define-type-predicate 'procedure? iltype-procedure)
;(define-predicate 'procedure? "isProcedure")

;; exact? ;; expect number
;; inexact? ;; expect number

(define (define-eq-predicate op value)
  (define-operation 1 op
    (lambda (as)
      (emit as
            (il:load-register 'result)
            (il:load-constant value)
            (il 'ceq)
            (rep:make-boolean)
            (il:set-register/pop 'result)))))
(define-eq-predicate 'null? '())
(define-eq-predicate 'unspecified? (unspecified))
(define-eq-predicate 'eof-object? (eof-object))
(define-eq-predicate 'undefined? (undefined))

(define (define-datum-op op value)
  (define-operation 1 op
    (lambda (as)
      (emit as (il:set-register 'result (il:load-constant value))))))
(define-datum-op 'unspecified (unspecified))
(define-datum-op 'undefined (undefined))
(define-datum-op 'eof-object (eof-object))

;; Data
;
;; Cell Data

(define-operation 1 'make-cell
  (lambda (as)
    (emit as
          (il:load-register 'result)
          (il:load-constant #f)
          (rep:make-pair)
          (il:set-register/pop 'result))))
;(define-operation 1 'cell-ref
;   (lambda (as)
;     (emit as
;           (il:load-register 'result)
;           (il 'castclass iltype-schemepair)
;           (rep:pair-car)
;           (il:set-register/pop 'result))))
;(define-operation 2 'cell-set!
;   (lambda (as reg2)
;     (emit as
;           (il:load-register 'result)
;           (il 'castclass iltype-schemepair)
;           (il:load-register reg2)
;           (rep:set-pair-car!)
;           (il:load-constant (unspecified))
;           (il:set-register/pop 'result))))

;; List Data

(define-operation 2 'cons
   (lambda (as reg2)
     (emit as
           (il:load-register 'result)
           (il:load-register reg2)
           (rep:make-pair)
           (il:set-register/pop 'result))))
;(define-operation 1 'car
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.car))
;          (rep:pair-car)
;          (il:set-register/pop 'result))))
;(define-operation 1 'car:pair
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il 'castclass iltype-schemepair)
;          (rep:pair-car)
;          (il:set-register/pop 'result))))
;(define-operation 1 'cdr
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.cdr))
;          (rep:pair-cdr)
;          (il:set-register/pop 'result))))
;(define-operation 1 'cdr:pair
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il 'castclass iltype-schemepair)
;          (rep:pair-cdr)
;          (il:set-register/pop 'result))))
;(define-operation 2 'set-car!
;  (lambda (as reg2)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.setcar))
;          (il:load-register reg2)
;          (rep:set-pair-car!)
;          (il:set-register 'result (il:load-constant (unspecified))))))
;(define-operation 2 'set-car!:pair
;  (lambda (as reg2)
;    (emit as
;          (il:load-register 'result)
;          (il 'castclass iltype-schemepair)
;          (il:load-register reg2)
;          (rep:set-pair-car!)
;          (il:set-register 'result (il:load-constant (unspecified))))))
;(define-operation 2 'set-cdr!
;  (lambda (as reg2)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.setcar))
;          (il:load-register reg2)
;          (rep:set-pair-cdr!)
;          (il:set-register 'result (il:load-constant (unspecified))))))
;(define-operation 2 'set-cdr!:pair
;  (lambda (as reg2)
;    (emit as
;          (il:load-register 'result)
;          (il 'castclass iltype-schemepair)
;          (il:load-register reg2)
;          (rep:set-pair-cdr!)
;          (il:set-register 'result (il:load-constant (unspecified))))))

;; Fixnum Data

(define-eq-predicate 'fxzero? 0)

;(define-operation 'fxpositive?
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-fixnum (il:abort-fail $ex.fxpositivep))
;          (rep:fixnum-value)
;          (il 'ldc.i4 0)
;          (il 'cgt)
;          (rep:make-boolean)
;          (il:set-register/pop 'result))))
;(define-operation 'fxnegative?
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-fixnum (il:abort-fail $ex.fxpositivep))
;          (rep:fixnum-value)
;          (il 'ldc.i4 0)
;          (il 'clt)
;          (rep:make-boolean)
;          (il:set-register/pop 'result))))

;; could do 2-operand fixnum arithmetic here...

;; Operations introduced by peephole optimizer

(define (define-branchf-eq-operation code object)
  (define-peephole-operation code
    (lambda (as target-label)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register 'result)
              (il:load-constant object)
              (il:branch-s 'beq no-branch-label)
              (il:br/use-fuel target-label)
              (il:label no-branch-label))))))
(define-branchf-eq-operation 'internal:branchf-null? '())
(define-branchf-eq-operation 'internal:branchf-eof-object? (eof-object))
(define-branchf-eq-operation 'internal:branchf-fxzero? 0)

(define (define-branchf-pred-operation code method)
  (define-peephole-operation code
    (lambda (as target-label)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register 'result)
              (il:call '(instance virtual) iltype-bool il-schemeobject method '())
              (il:branch-s 'brtrue no-branch-label)
              (il:br/use-fuel target-label)
              (il:label no-branch-label))))))
(define-branchf-pred-operation 'internal:branchf-pair? "isPair")
(define-branchf-pred-operation 'internal:branchf-fixnum? "isFixnum")
(define-branchf-pred-operation 'internal:branchf-char? "isChar")

(define (define-reg/op1/check-operation code method)
  (define-peephole-operation code
    (lambda (as reg target-label unused-triplet)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register reg)
              (il:call '(instance virtual) iltype-bool il-schemeobject method '())
              (il:branch 'brfalse target-label))))))
(define-reg/op1/check-operation 'internal:check-fixnum? "isFixnum")
(define-reg/op1/check-operation 'internal:check-pair? "isPair")
(define-reg/op1/check-operation 'internal:check-vector? "isVector")
(define-reg/op1/check-operation 'internal:check-string? "isString")

;; /Operations
