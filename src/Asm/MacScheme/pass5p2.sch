; Copyright 1991 Lightship Software, Incorporated.
;
; MacScheme machine assembler.

(define $bytecode-assembly-table$
  (make-vector
   64
   (lambda (instruction as)
     (error "Unrecognized mnemonic" instruction))))

(define (define-instruction i proc)
  (vector-set! $bytecode-assembly-table$ i proc)
  #t)

(define (define-code . bytes)
  (list->bytevector bytes))

(define (primop-code name)
  (prim-primcode (prim-entry name)))

(define (immediate-primop-code name)
  (cadr (assq name $immediate-primops$)))

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 8 (string-length name)))
                                   #\space))
             (if (not (null? (cdr instruction)))
                 (begin (write (cadr instruction))
                        (do ((operands (cddr instruction)
                                       (cdr operands)))
                            ((null? operands))
                            (write-char #\,)
                            (write (car operands)))))
             (newline))))

(define (list-label instruction)
  (if listify?
      (begin (display list-indentation)
             (write-char #\L)
             (write (cadr instruction))
             (newline))))

(define (list-lambda-start instruction)
  (list-instruction "lambda" (list $lambda '* (operand2 instruction)))
  (set! list-indentation (string-append list-indentation "|   ")))

(define (list-lambda-end)
  (set! list-indentation
        (substring list-indentation
                   0
                   (- (string-length list-indentation) 4))))

(define list-indentation "")

(define listify? #t)

(define $cons 'cons)

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (emit-label! as (operand1 instruction))))

(define-instruction $.proc
  ; Interpreted procedures begin with a jsr xx(yy).
  (let ((bits (define-code #b01001110 #b10101000 #xff #xff)))
    (lambda (instruction as)
      (list-instruction ".proc" instruction)
      (if (not (zero? (remainder (here as) 4)))
          (error "Unaligned .proc"))
      (emit! as bits))))

(define-instruction $.cont
  ; Interpreted return points begin with a jsr xx(yy).
  (let ((bits (define-code #b01001110 #b10101000 #xff #xff)))
    (lambda (instruction as)
      (list-instruction ".cont" instruction)
      (if (not (zero? (remainder (here as) 4)))
          (error "Unaligned .cont"))
      (emit! as bits))))

(define-instruction $.align
  (lambda (instruction as)
    (if (not (zero? (remainder (here as)
                               (operand1 instruction))))
        (begin (push-instruction as instruction)
               (push-instruction as (cg-op $nop))))))

; Instructions.

; A hack to deal with the MacScheme macro expander's treatment of
; 1+ and 1-.

(define-instruction $op1
  (let ((bits (define-code #x5e 0)))
    (lambda (instruction as)
      (cond ((eq? (operand1 instruction) '1+)
             (push-instruction as (cg-op $opx '+ 1)))
            ((eq? (operand1 instruction) '1-)
             (push-instruction as (cg-op $opx '- 1)))
            (else
             (list-instruction "op1" instruction)
             (emit-fixup! as 1 1 (primop-code (operand1 instruction)))
             (emit! as bits))))))

(define-instruction $op2
  (let ((bits (define-code #x60 0)))
    (lambda (instruction as)
      (list-instruction "op2" instruction)
      (emit-fixup! as 1 1 (primop-code (operand1 instruction)))
      (emit-fixup! as 0 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $op3
  (let ((bits (define-code #x80 0 0)))
    (lambda (instruction as)
      (list-instruction "op3" instruction)
      (emit-fixup! as 1 1 (primop-code (operand1 instruction)))
      (emit-fixup! as 0 1 (operand2 instruction))
      (emit-fixup! as 2 1 (operand3 instruction))
      (emit! as bits))))

(define-instruction $op2imm
  (let ((bits (define-code #x5f 0 0))
        (immediate-operand
         (lambda (x)
           (cond ((fixnum? x) x)
                 ((char? x) (char->integer x))
                 (else ???)))))
    (lambda (instruction as)
      (list-instruction "op2imm" instruction)
      (emit-fixup! as 1 1 (immediate-primop-code (operand1 instruction)))
      (emit-fixup! as 2 1 (immediate-operand (operand2 instruction)))
      (emit! as bits))))

(define-instruction $const
  (let ((bits        (define-code #x5c 0 0))
        (fixnum      (define-code #x5d 0))
        (false       (define-code #xa0))
        (true        (define-code #xa1))
        (null        (define-code #xa2))
        (unspecified (define-code #xa3))
        (eof         (define-code #xa4)))
    (lambda (instruction as)
      (list-instruction "const" instruction)
      (let ((x (operand1 instruction)))
        (cond ((and (fixnum? x)
                    (<= 0 x 255))
               (emit-fixup! as 1 1 x)
               (emit! as fixnum))
              ((eq? x #f) (emit! as false))
              ((eq? x #t) (emit! as true))
              ((eq? x '()) (emit! as null))
              ((equal? x hash-bang-unspecified) (emit! as unspecified))
              ((eq? x **eof**) (emit! as eof))
              (else
               (emit-fixup! as 1 2 (emit-constant as x))
               (emit! as bits)))))))

(define-instruction $global
  (let ((bits (define-code #x5a 0 0)))
    (lambda (instruction as)
      (list-instruction "global" instruction)
      ; This is the wrong thing.
      (emit-fixup! as 1 2 (emit-constant as (operand1 instruction)))
      (emit! as bits))))

(define-instruction $setglbl
  (let ((bits (define-code #x5b 0 0)))
    (lambda (instruction as)
      (list-instruction "setglbl" instruction)
      ; This is the wrong thing.
      (emit-fixup! as 1 2 (emit-constant as (operand1 instruction)))
      (emit! as bits))))

(define-instruction $lambda
  (let ((bits (define-code #xb5 0 0 0)))
    (lambda (instruction as)
      (list-lambda-start instruction)
      (let ((segment (assemble-nested-lambda as (operand1 instruction))))
        (list-lambda-end)
        (emit-fixup! as 1 2 (emit-constants as (car segment)
                                               (cdr segment)
                                               (operand3 instruction)))
        (emit-fixup! as 3 1 (operand2 instruction))
        (emit! as bits)))))

(define-instruction $lexes
  (let ((bits (define-code #xb4 0 0 0)))
    (lambda (instruction as)
      (list-instruction "lexes" instruction)
      (emit-fixup! as 3 1 (operand1 instruction))
      (emit-fixup! as 1 2 (emit-constant as (operand2 instruction)))
      (emit! as bits))))

(define-instruction $args=
  (let ((bits (define-code #x50 0)))
    (lambda (instruction as)
      (list-instruction "args=" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $args>=
  (let ((bits (define-code #x51 0)))
    (lambda (instruction as)
      (list-instruction "args>=" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $invoke
  (let ((bits (define-code #x55 0)))
    (lambda (instruction as)
      (list-instruction "invoke" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $restore
  (let ((bits (define-code #x56 0)))
    (lambda (instruction as)
      (list-instruction "restore" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $pop
  (let ((bits (define-code #x57 0)))
    (lambda (instruction as)
      (list-instruction "pop" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $stack
  (let ((bits (define-code #x52 0)))
    (lambda (instruction as)
      (list-instruction "stack" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $setstk
  (let ((bits (define-code #x53 0)))
    (lambda (instruction as)
      (list-instruction "setstk" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $load
  (let ((bits (define-code #xb0 0 0)))
    (lambda (instruction as)
      (list-instruction "load" instruction)
      (emit-fixup! as 2 1 (operand1 instruction))
      (emit-fixup! as 1 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $store
  (let ((bits (define-code #xb1 0 0)))
    (lambda (instruction as)
      (list-instruction "store" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit-fixup! as 2 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $lexical
  (let ((bits (define-code #xb2 0 0)))
    (lambda (instruction as)
      (list-instruction "lexical" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit-fixup! as 2 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $setlex
  (let ((bits (define-code #xb3 0 0)))
    (lambda (instruction as)
      (list-instruction "setlex" instruction)
      (emit-fixup! as 1 1 (operand1 instruction))
      (emit-fixup! as 2 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $reg
  (let ((bits (define-code #xc0)))
    (lambda (instruction as)
      (list-instruction "reg" instruction)
      (emit-fixup! as 0 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $setreg
  (let ((bits (define-code #xe0)))
    (lambda (instruction as)
      (list-instruction "setreg" instruction)
      (emit-fixup! as 0 1 (operand1 instruction))
      (emit! as bits))))

(define-instruction $movereg
  (let ((bits (define-code #x20 0)))
    (lambda (instruction as)
      (list-instruction "movereg" instruction)
      (emit-fixup! as 0 1 (operand1 instruction))
      (emit-fixup! as 1 1 (operand2 instruction))
      (emit! as bits))))

(define-instruction $return
  (let ((bits (define-code #x10)))
    (lambda (instruction as)
      (list-instruction "return" instruction)
      (emit! as bits))))

(define-instruction $apply
  (let ((bits (define-code #x12)))
    (lambda (instruction as)
      (list-instruction "apply" instruction)
      (emit! as bits))))

(define-instruction $nop
  (let ((bits (define-code #x1f)))
    (lambda (instruction as)
      (list-instruction "nop" instruction)
      (emit! as bits))))

(define-instruction $jump
  (let ((bits (define-code #xb7 0 0 0)))
    (lambda (instruction as)
      (list-instruction "jump" instruction)
      (emit-fixup! as 3 1 (operand1 instruction))
      (emit-fixup-label! as 1 2 (operand2 instruction))
      (emit! as bits))))

(define-instruction $save
  (let ((bits (define-code #xb6 0 0 0)))
    (lambda (instruction as)
      (list-instruction "save" instruction)
      (emit-fixup-label! as 1 2 (operand1 instruction))
      (emit-fixup! as 3 1 (operand2 instruction))
      (emit! as bits)
      (emit-fixup! as -3 2 (- (here as))))))

(define-instruction $setrtn
  (let ((bits (define-code #xb8 0 0)))
    (lambda (instruction as)
      (list-instruction "setrtn" instruction)
      (emit-fixup-label! as 1 2 (operand1 instruction))
      (emit! as bits)
      (emit-fixup! as -2 2 (- (here as))))))

; In interpreted byte code there is an important difference
; between the skip instruction and the branch instruction.
; The branch instruction is used only for calls to known
; procedures, which begin with 4 bytes of native code that
; an interpreted branch must branch past.

(define-instruction $skip
  (let ((bits (define-code #xba 0 0)))
    (lambda (instruction as)
      (list-instruction "skip" instruction)
      (emit-fixup-label! as 1 2 (operand1 instruction))
      (emit! as bits)
      (emit-fixup! as -2 2 (- (here as))))))

(define-instruction $branch
  (let ((bits (define-code #xba 0 4)))  ; see above
    (lambda (instruction as)
      (list-instruction "branch" instruction)
      (emit-fixup-label! as 1 2 (operand1 instruction))
      (emit! as bits)
      (emit-fixup! as -2 2 (- (here as))))))

(define-instruction $branchf
  (let ((bits (define-code #xbf 0 0)))
    (lambda (instruction as)
      (list-instruction "branchf" instruction)
      (emit-fixup-label! as 1 2 (operand1 instruction))
      (emit! as bits)
      (emit-fixup! as -2 2 (- (here as))))))
