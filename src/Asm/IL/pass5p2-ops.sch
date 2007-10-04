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
(define (opX argc immediate? reg/setreg?)
  (lambda (instruction as)
    (let ((fmt (if reg/setreg?
                   (if immediate? "reg/op~aimm/setreg" "reg/op~a/setreg")
                   (if immediate? "op~aimm" "op~a"))))
      (list-instruction/line (twobit-format #f fmt argc) instruction as))
    (let ((primop (operand1 instruction))
          (operands (if reg/setreg? 
                        (cddr instruction) 
                        (append '(result result) (cddr instruction)))))
      (cond ((and (not immediate?) (lookup-operation argc primop))
             => (lambda (h) 
                  (emit as (il:comment "operation ~s" primop))
                  (apply (cdr h) as operands)))
            ((and immediate? (lookup-imm2-operation primop))
             => (lambda (h) 
                  (emit as (il:comment "operation ~s" primop))
                  (apply (cdr h) as operands)))
            (else
             (cond (reg/setreg? (error 'opX)))
             (opX/rt argc immediate? instruction as))))))

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
	      (il:comment "operation ~s" (operand1 instruction))
              load-arguments-code
              (il:set-register 'result
                               (il:call-opX (operand1 instruction) argc #f))))))

(define opX-implicit-continuation? op1-implicit-continuation?)

(define (peephole-operation opcode)
  (lambda (instruction as)
    (let ((primop (operand1 instruction)))
      (cond ((lookup-peephole-operation primop)
             => (lambda (h) 
                  (emit as 
                        (il:comment "instruction ~s" opcode)
                        (il:comment "operation ~s" primop))
                  (apply (cdr h) as (cddr instruction))))
            (else (error "Operation not supported: " instruction))))))

;; Instructions

(define-instruction $op1
  (opX 1 #f #f))

(define-instruction $op2
  (opX 2 #f #f))

(define-instruction $op2imm
  (opX 2 #t #f))

(define-instruction $op3
  (opX 3 #f #f))

(define-instruction $reg/op1/setreg
  (opX 1 #f #t))

(define-instruction $reg/op2/setreg
  (opX 2 #f #t))

(define-instruction $reg/op2imm/setreg
  (opX 2 #t #t))

(define-instruction $reg/op1/branchf
  (peephole-operation 'reg/op1/branchf))

(define-instruction $reg/op2/branchf
  (peephole-operation 'reg/op2/branchf))

(define-instruction $reg/op2imm/branchf
  (peephole-operation 'reg/op2imm/branchf))

(define-instruction $reg/op1/check
  (peephole-operation 'reg/op1/check))

(define-instruction $reg/op2/check
  (peephole-operation 'reg/op2/check))

(define-instruction $reg/op2imm/check
  (peephole-operation 'reg/op2imm/check))

;; -----------------
;; Operations
;; -----------------

(define-operation 1 'creg
  (lambda (as rs rd)
    (emit as
          (il:call '() iltype-schemeobject il-cont "getCC" '())
          (il:set-register/pop rd))))
(define-operation 1 'creg-set!
  (lambda (as rs rd)
    (emit as
          (il:load-register rs)
          (il:call '() iltype-void il-cont "setCC" (list iltype-schemeobject))
          (il:set-register rd (il:load-constant (unspecified))))))
(define-operation 1 'break
  (lambda (as rs rd)
    (emit as
          (il:fault $ex.breakpoint))))
(define-operation 1 'gc-counter
  (lambda (as rs rd)
    (emit as
          (il:load-constant 0)
          (il:set-register/pop rd))))
(define-operation 1 'not
  (lambda (as rs rd)
    (emit as
          (il:load-register rs)
          (il:load-constant #f)
          (il 'ceq)
          (rep:make-boolean)
          (il:set-register/pop rd))))

;; Predicates

(define-operation 2 'eq?
  (lambda (as rs1 rd rs2)
    (emit as
          (il:load-register rs1)
          (il:load-register rs2)
          (il 'ceq)
          (rep:make-boolean)
          (il:set-register/pop rd))))

(define-imm2-operation 'eq? 
  (lambda (as rs1 rd imm)
    (emit as
          (il:load-register rs1)
          (il:load-constant imm)
          (il 'ceq)
          (rep:make-boolean)
          (il:set-register/pop rd))))

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
    (lambda (as rs rd)
      (emit as
            (il:load-register rs)
            (il:call '(instance virtual) iltype-bool il-schemeobject method '())
            (rep:make-boolean)
            (il:set-register/pop rd)))))

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
    (lambda (as rs rd)
      (emit as
            (il:load-register rs)
            (il:load-constant value)
            (il 'ceq)
            (rep:make-boolean)
            (il:set-register/pop rd)))))
(define-eq-predicate 'null? '())
(define-eq-predicate 'unspecified? (unspecified))
(define-eq-predicate 'eof-object? (eof-object))
(define-eq-predicate 'undefined? (undefined))

(define (define-datum-op op value)
  (define-operation 1 op
    (lambda (as rs rd)
      (emit as (il:set-register rd (il:load-constant value))))))
(define-datum-op 'unspecified (unspecified))
(define-datum-op 'undefined (undefined))
(define-datum-op 'eof-object (eof-object))

;; Data
;
;; Cell Data

(define-operation 1 'make-cell
  (lambda (as rs rd)
    (emit as
          (il:load-register rs)
          (il:load-constant #f)
          (rep:make-pair)
          (il:set-register/pop rd))))
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
   (lambda (as rs1 rd rs2)
     (emit as
           (il:load-register rs1)
           (il:load-register rs2)
           (rep:make-pair)
           (il:set-register/pop rd))))
;(define-operation 1 'car
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.car))
;          (rep:pair-car)
;          (il:set-register/pop 'result))))
(define-operation 1 'car:pair
  (lambda (as rs rd)
    (emit as
          (il:load-register rs)
          (il 'castclass iltype-schemepair)
          (rep:pair-car)
          (il:set-register/pop rd))))
;(define-operation 1 'cdr
;  (lambda (as)
;    (emit as
;          (il:load-register 'result)
;          (il:check-type iltype-schemepair
;                         (il:fault-abort $ex.cdr))
;          (rep:pair-cdr)
;          (il:set-register/pop 'result))))
(define-operation 1 'cdr:pair
  (lambda (as rs rd)
    (emit as
          (il:load-register rs)
          (il 'castclass iltype-schemepair)
          (rep:pair-cdr)
          (il:set-register/pop rd))))
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
(define (define-reg/setreg-op1 op op_method)
  (define-operation 1 op
    (lambda (as rs rd)
      (emit as
            (il:load-register rs)
            (il:call '(instance virtual) iltype-schemeobject il-schemeobject
                     op_method '())
            (il:set-register/pop rd)))))
(define-reg/setreg-op1 'char->integer     "op_char2integer")
(define-reg/setreg-op1 'vector-length:vec "op_vector_length_vec")
(define-reg/setreg-op1 'char?             "op_charp")
(define-reg/setreg-op1 'cell-ref          "op_cell_ref")

;; could do 2-operand fixnum arithmetic here...

(define (il:br/maybe-use-fuel as target-label)
  (if (assq target-label (as-labels as))
      (il:br/use-fuel target-label)
      (il:branch 'br target-label)))

;; Operations introduced by peephole optimizer

(define (define-branchf-eq-operation code object)
  (define-peephole-operation code
    (lambda (as rs target-label)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register rs)
              (il:load-constant object)
              (il:branch-s 'beq no-branch-label)
              (il:br/maybe-use-fuel as target-label)
              (il:label no-branch-label))))))
(define-branchf-eq-operation 'internal:branchf-null? '())
(define-branchf-eq-operation 'internal:branchf-eof-object? (eof-object))
(define-branchf-eq-operation 'internal:branchf-fxzero? 0)

(define-peephole-operation 'internal:branchf-eq?
  (lambda (as rs1 rs2 target-label)
    (let ((no-branch-label (allocate-label as)))
      (emit as 
            (il:load-register rs1)
            (il:load-register rs2)
            (il:branch-s 'beq no-branch-label)
            (il:br/maybe-use-fuel as target-label)
            (il:label no-branch-label)))))

(define (define-branchf-pred-operation code method)
  (define-peephole-operation code
    (lambda (as rs target-label)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register rs)
              (il:call '(instance virtual) iltype-bool il-schemeobject method '())
              (il:branch-s 'brtrue no-branch-label)
              (il:br/maybe-use-fuel as target-label)
              (il:label no-branch-label))))))
(define-branchf-pred-operation 'internal:branchf-pair? "isPair")
(define-branchf-pred-operation 'internal:branchf-fixnum? "isFixnum")
(define-branchf-pred-operation 'internal:branchf-char? "isChar")

(define (define-branchf-pred-imm-int32-operation code orig-code method)
  (define-peephole-operation code 
    (lambda (as rs imm target-label)
      (cond ((opX-implicit-continuation? orig-code)
             (error 'define-branchf-pred-imm-int32-operation
                    ": op " code " needs an implicit continuation...")))
      (let ((no-branch-label (allocate-label as)))
        (emit as 
              (il:load-register rs)
              (il 'ldc.i4 imm)
              (il:call '(instance virtual) iltype-bool il-schemeobject 
                       method (list iltype-int32))
              (il:branch-s 'brtrue no-branch-label)
              (il:br/maybe-use-fuel as target-label)
              (il:label no-branch-label))))))
(define-branchf-pred-imm-int32-operation 'internal:branchf-eq?/imm-int32 
  'eq? "isEqpInt32")
(define-branchf-pred-imm-int32-operation 'internal:branchf-fx</imm-int32 
  'fx< "isFxLessInt32")
(define-branchf-pred-imm-int32-operation 'internal:branchf-=:fix:fix/imm-int32 
  '=:fix:fix "isNumericEqualFixFixInt32")
(define-branchf-pred-imm-int32-operation 'internal:branchf-<:fix:fix/imm-int32 
  '<:fix:fix "isLessFixFixInt32")

(define (define-branchf-pred-imm-char-operation code orig-code method)
  (define-peephole-operation code
    (lambda (as rs char-imm target-label)
      (let ((no-branch-label (allocate-label as)))
        (emit as
              (il:load-register rs)
              (il 'ldc.i4 (char->integer char-imm))
              (il:call '(instance virtual) iltype-bool il-schemeobject
                       method (list iltype-int32))
              (il:branch-s 'brtrue no-branch-label)
              (il:br/maybe-use-fuel as target-label)
              (il:label no-branch-label))))))
(define-branchf-pred-imm-char-operation 'internal:branchf-char=?/imm-char
  'char=? "isCharEqualsInt32")

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

(define (define-op2imm-int32-operation code orig-code method)
  (define-imm2-operation code
    (lambda (as rs rd const)
      (cond 
       ((opX-implicit-continuation? orig-code)
        ;; this is complex b/c generic arith ops only work via result reg.  :(
        (cond ((not (eqv? rs 'result))
               (emit as 
                     (il:call '() iltype-void il-instructions
                              (string-append "reg" (number->string rs))
                              '()))))
        (let ((numeric (allocate-label as)))
          (emit as
                (il:comment "  implicit continuation LABEL ~s; JUMP INDEX ~s"
                            numeric (intern-label as numeric))
                (rep:set-implicit-continuation numeric)
                (il:load-register rs)
                (il 'ldc.i4 const)
                (il:call '(instance virtual) iltype-void il-schemeobject
                         method (list iltype-int32))
                ;; if codegen were correct, is this reset actually necessary???
                (rep:reset-implicit-continuation) 
                (il:label numeric)))
        (cond ((not (eqv? rd 'result))
               ;; UGH.  Might be better to not bother opX/setreg on these.
               (emit as 
                     (il:call '() iltype-void il-instructions
                              (string-append "setreg" (number->string rd))
                              '())))))
       (else
        (emit as
              (il:load-register rs)
              (il 'ldc.i4 const)
              (il:call '(instance virtual) iltype-schemeobject il-schemeobject
                       method (list iltype-int32))
              (il:set-register/pop rd)))))))

(define-op2imm-int32-operation 'eq?:int32 'eq?
  "op_eqp_int32")
(define-op2imm-int32-operation '+:idx:idx:int32 '+:idx:idx 
  "op_plus_idx_idx_int32")
(define-op2imm-int32-operation '-:idx:idx:int32 '-:idx:idx 
  "op_minus_idx_idx_int32")
(define-op2imm-int32-operation '=:int32 '=                   
  "op_numeric_equals_int32")
(define-op2imm-int32-operation '+:int32 '+ 
  "op_plus_int32")
(define-op2imm-int32-operation '-:int32 '- 
  "op_minus_int32")
(define-op2imm-int32-operation 'fx<:int32 'fx<               
  "op_fxless_int32")
(define-op2imm-int32-operation '>=:fix:fix:int32 '>=:fix:fix 
  "op_greaterequal_fix_fix_int32")
(define-op2imm-int32-operation '<:fix:fix:int32 '<:fix:fix   
  "op_less_fix_fix_int32")
(define-op2imm-int32-operation 'vector-ref:trusted:int32 'vector-ref:trusted
  "op_vector_ref_trusted_int32")

(define-imm2-operation 'char=?:char
  (lambda (as rs rd const)
    (emit as
          (il:load-register rs)
          (il 'ldc.i4 (char->integer const))
          (il:call '(instance virtual) iltype-schemeobject il-schemeobject
                   "op_charequals_int32" (list iltype-int32))
          (il:set-register/pop rd))))

;; /Operations
