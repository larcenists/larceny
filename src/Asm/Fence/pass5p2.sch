; Copyright 1991 Lightship Software, Incorporated.
;
; 23 August 2012 / lth@acm.org
;
; Asm/Fence/pass5p2.sch -- Fence machine assembler, top level
;
; The fence translates MacScheme machine language (high-level
; intermediate language) to Cant (low-level intermediate language).  A
; translator from Cant to a target machine language is simple yet
; amenable to optimization, see eg pass5p2-arm.sch.
;
; We make some simplifying assumptions:
;  - Load-store architecture
;  - Floating-point register file (not an 8087-like stack)
;  - "Appropriate" alignment of data and 8-byte alignment of stack frames
;  - GLOBALS, STKP, and SECOND are stored in hardware registers
;  - SECOND is used extensively as a temporary
;  - The return address is always stored in the frame (never in LR)
;  - TIMER, ETOP, and THIRD are stored in the globals array
;  - Exceptions in generated code are all non-continuable
;
; Features:
;  - Little/big-endian independent.
;  - By and large it is 64-bit clean.
;  - Direct support for floating point operations.
;
; TODO:
;  - Sundry local improvements, search for TODO below
;  - Support more switches, notably inline allocation, inline barrier.
;  - EMIT-EXCEPTION should maintain a table of in-line exception 
;    handlers (registers moved and exception triggered), and emit
;    branches to existing handlers instead of new handlers; the
;    optimizer should carefully optimize branch-over-branch to
;    reverse the condition and eliminate the redundant branch.
;  - Add the missing parts of the 64-bit support, search for FIXME64.

; Arguably a hack, should define these elsewhere.  But they are
; target-independent.

(define $r.f0 0)
(define $r.f1 1)
(define $r.f2 2)
(define $r.f3 3)

; Overrides the procedure of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $fence-assembly-table$)

; Controls listing of instructions during assembly.

(define listify? #f)
(define listify-cant? #f)

; Table of assembler procedures.

(define $fence-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $fence-assembly-table$ i proc)
  #t)

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 12 (string-length name)) 1)
                                   #\space))
             (if (not (null? (cdr instruction)))
                 (begin (write (cadr instruction))
                        (do ((operands (cddr instruction)
                                       (cdr operands)))
                            ((null? operands))
                            (write-char #\,)
                            (write-char #\space)
                            (if (symbol? (car operands)) ; Else .n|3 etc are unreadable
                                (display (car operands))
                                (write (car operands))))))
             (newline)
             (flush-output-port))))

(define (list-label instruction)
  (if listify?
      (begin (display list-indentation)
             (write-char #\L)
             (write (cadr instruction))
             (newline))))

(define (list-asm-label L)
  (if listify-cant?
      (begin (display list-indentation)
             (write-char #\Q)
             (write (- (caar L)))
             (newline))))

(define (list-lambda-start instruction)
  (list-instruction "lambda" (list $lambda '* (operand2 instruction)))
  (set! list-indentation (string-append list-indentation "|   ")))

(define (list-lambda-end)
  (set! list-indentation
        (substring list-indentation
                   0
                   (- (string-length list-indentation) 4))))

; List a Cant instruction, indented from the MAL instruction.

(define (list-cant name instruction)
  (if listify-cant?
      (list-instruction (string-append "  " name) instruction)))

; Format a label for listing

(define (list-labelname L)
  (string->symbol
   (if (fence.label? L)
       (string-append "Q" (number->string (- (caar L))))
       (string-append "L" (number->string (car L))))))

; Format a register for listing

(define (list-regname r)
  (cond ((= r $r.result)  'RESULT)
        ((= r $r.second)  'SECOND)
        ((= r $r.stkp)    'STKP)
        ((= r $r.globals) 'GLOBALS)
        ((= r $r.tmp)     'TMP)
        (else (string->symbol (string-append "REG" (number->string r))))))

(define (list-fregname r)
  (string->symbol (string-append "F" (number->string r))))

(define list-indentation "")

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (cant.label as (make-asm-label as (operand1 instruction)))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)))

(define-instruction $.proc-doc
  (lambda (instruction as)
    (list-instruction ".proc-doc" instruction)
    (add-documentation as (operand1 instruction))))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)))

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    (let loop ()
      (if (not (zero? (remainder (here as) $bytewidth.code-align)))
          (begin
            (cant.nop as)
            (loop))))))

(define-instruction $.end
  (lambda (instruction as)
    (cant.end as)))

; Single-step non-trivial MAL instructions.
;
; This all happens in millicode.  Pass the index of the documentation
; string in SECOND.  Some instructions execute when reg0 is not a
; valid pointer to the current procedure (because this is just after
; returning); in this case we restore reg0 from the stack before
; calling millicode.

(define-instruction $.singlestep
  (lambda (instruction as)
    (let ((instr (car (as-source as))))
      
      (define (special?)
        (let ((op (operand0 instr)))
          (or (= op $.label)
              (= op $.proc)
              (= op $.proc-doc)
              (= op $.end)
              (= op $.align)
              (and (= op $load) (= 0 (operand1 instr))))))
      
      (define (readify-instr)
        (if (= (operand0 instr) $lambda)
            (list 'lambda '(...) (caddr instr) (cadddr instr))
            (car (readify-lap (list instr)))))
      
      (if (not (special?))
          (let ((repr   (format-object (readify-instr)))
                (funky? (= (operand0 instr) $.cont)))
            (if funky?
                (cant.ldi as $r.stkp (fence.frame-slot 0) $r.reg0))
            (emit-constant->register as repr $r.second)
            (cant.trap as $m.singlestep '*singlestep))))))

; Instructions.

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)))

(define-instruction $op1
  (lambda (instruction as)
    (list-instruction "op1" instruction)
    (emit-op1 as (operand1 instruction) $r.result $r.result)))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-op2 as (operand1 instruction) $r.result (operand2 instruction) $r.result)))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-op3 as (operand1 instruction) $r.result (operand2 instruction) (operand3 instruction))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (emit-op2imm as (operand1 instruction) $r.result (operand2 instruction) $r.result)))

(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (emit-constant->register as (operand1 instruction) $r.result)))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-getglbl as (operand1 instruction) $r.result)))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-setglbl as $r.result (operand1 instruction))))

(define-instruction $lambda
  (lambda (instruction as)
    (let ((code-offset  #f)
          (const-offset #f))
      (list-lambda-start instruction)
      (assemble-nested-lambda as
                              (operand1 instruction)
                              (operand3 instruction)   ; documentation
                              (lambda (nested-as segment)
                                (set-constant! as code-offset (car segment))
                                (set-constant! as const-offset (cdr segment))))
      (list-lambda-end)
      (let ((n (operand2 instruction)))
        (set! code-offset  (emit-codevector as 0))
        (set! const-offset (emit-constantvector as 0))
        (emit-allocate-procedure as n)  ; Object in $r.result; uninialized apart from the header
        (emit-load-constant as code-offset $r.second)
        (cant.sti as $r.second $r.result $p.codevector)
        (emit-load-constant as const-offset $r.second)
        (cant.sti as $r.second $r.result $p.constvector)
        (emit-init-procedure-slots as n)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (let ((n (operand1 instruction)))
      (emit-allocate-procedure as n)     ; Object in $r.result; uninialized apart from the header
      (cant.ldi as $r.reg0 $p.codevector $r.second)
      (cant.sti as $r.second $r.result $p.codevector)
      (cant.ldi as $r.reg0 $p.constvector $r.second)
      (cant.sti as $r.second $r.result $p.constvector)
      (emit-init-procedure-slots as n))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (let ((L1 (fence.make-label as))
          (n  (fence.native->fixnum (operand1 instruction))))
      (cant.bcci   as 'equal $r.result n L1)
      (cant.movi   as n $r.second)
      (cant.trap   as $m.argc-ex '*argc-ex) ; THIRD=REG0 happens in millicode
      (fence.label as L1))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (let ((L1 (fence.make-label as))
          (n  (fence.native->fixnum (operand1 instruction))))
      (cant.movi as n $r.second)
      (if (runtime-safety-checking)
          (begin
            (cant.bcc  as 'greater-or-equal $r.result $r.second L1)
            (emit-exception as $ex.vargc $r.result $r.second) ; THIRD=REG0 happens in millicode
            (fence.label as L1)))
      (cant.trap as $m.varargs '*varargs))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-invoke as $r.result (operand1 instruction) $m.invoke-ex '*invoke-ex)))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (cant.ldrtn/return as $stk.retaddr)))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (cant.setrtn/strtn as (make-asm-label as (operand1 instruction)) $stk.retaddr)))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    ;; The apply instruction is trusted, so there is no type checking
    ;; of the operands.
    (emit-timer-tick as)
    (cant.mov  as (operand1 instruction) $r.second)
    (cant.sti  as (operand2 instruction) $r.globals $g.third)
    ;; The trap does not return, the control transfer is handled in millicode.
    ;; The reason it's in millicode is that there would be a lot of in-line code
    ;; for every instance of APPLY on register-rich platforms.
    (cant.trap as $m.apply '*apply)))

(define-instruction $save
  (lambda (instruction as)
    (let ((n (operand1 instruction)))
      (if (not (negative? n))
          (begin
           (list-instruction "save" instruction)
           (emit-save0 as n)
           (emit-save1 as n))))))

(define-instruction $restore
  (lambda (instruction as)
    (let ((n (operand1 instruction)))
      (if (not (negative? n))
          (begin
            (list-instruction "restore" instruction)
            (do ((i 0 (+ i 1)))
                ((> i n))
              (cant.ldi as $r.stkp (fence.frame-slot i) i)))))))

(define-instruction $pop
  (lambda (instruction as)
    (let ((n (operand1 instruction)))
      (if (not (negative? n))
          ;; n+3 words for retaddr, dynamic link, and REG0..REGn
          ;; realsize includes the size field and padding
          (let* ((framesize (* $bytewidth.wordsize (+ n 3)))
                 (realsize  (fence.roundup8
                             (+ framesize $bytewidth.wordsize))))
            (list-instruction "pop" instruction)
            (cant.addi as $r.stkp realsize $r.stkp))))))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (cant.ldi as $r.stkp (fence.frame-slot (operand1 instruction)) $r.result)))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (cant.sti as $r.result $r.stkp (fence.frame-slot (operand1 instruction)))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (cant.ldi as $r.stkp (fence.frame-slot (operand2 instruction)) (operand1 instruction))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (cant.sti as (operand1 instruction) $r.stkp (fence.frame-slot (operand2 instruction)))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (let* ((rib  (operand1 instruction))
           (slot (operand2 instruction))
           (base (emit-follow-lexical-chain as rib $r.second)))
      (cant.ldi as base (fence.procedure-slot slot) $r.result))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (let ((rib  (operand1 instruction))
          (slot (operand2 instruction)))
      ;; We either move the value here or it will need to be moved in
      ;; the write barrier invocation.  See notes at emit-write-barrier.
      (cant.mov as $r.result $r.second)
      (let ((base (emit-follow-lexical-chain as rib $r.result)))
        (cant.sti           as $r.second base (fence.procedure-slot slot))
        (emit-write-barrier as base $r.second)))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (cant.mov as (operand1 instruction) $r.result)))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (cant.mov as $r.result (operand1 instruction))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (cant.mov as (operand1 instruction) (operand2 instruction))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (let ((m (operand1 instruction))
          (L (make-asm-label as (operand2 instruction))))
      (emit-timer-tick as)
      (emit-follow-lexical-chain as m $r.reg0)
      (cant.jump as L))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (cant.ba as (make-asm-label as (operand1 instruction)))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (let ((L (make-asm-label as (operand1 instruction))))
      (if (label-value as L)
          (emit-timer-tick as))         ; Backward branch
      (cant.ba as L))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-branchf as $r.result (make-asm-label as (operand1 instruction)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction "check" instruction)
    (emit-check as $r.result (make-asm-label as (operand4 instruction)))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction "trap" instruction)
    (let ((r1  (operand1 instruction))
          (r2  (operand2 instruction))
          (r3  (operand3 instruction))
          (exn (operand4 instruction)))
      (if (not (= r3 0))
          (cant.sti as r3 $r.globals $g.third))
      (if (not (= r2 0))
          (cant.mov as r2 $r.second))
      (if (not (= r1 0))
          (cant.mov as r1 $r.result))
      (emit-exception as exn $r.result $r.second))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (emit-constant->register as (operand1 instruction) (operand2 instruction))))

; Instructions introduced by peephole optimization

(define-instruction $reg/setglbl        ; reg/setglbl r g
  (lambda (instruction as)
    (list-instruction "reg/setglbl" instruction)
    (emit-setglbl as (operand1 instruction) (operand2 instruction))))

(define-instruction $const/setglbl      ; const/setglbl c g
  (lambda (instruction as)
    (list-instruction "const/setglbl" instruction)
    (emit-constant->register as (operand1 instruction) $r.second)
    (emit-setglbl as $r.second (operand2 instruction))))

(define-instruction $global/setreg      ; global/setreg name r
  (lambda (instruction as)
    (list-instruction "global/setreg" instruction)
    (emit-getglbl as (operand1 instruction) (operand2 instruction))))

; This is RISC-centric.  The x86 favors (slightly?) a style that
; creates the frame by pushing slots in order, see the IAssassin code
; generator.

(define-instruction $save/stores        ; save/stores n (reg ...) (slot ...)
  (lambda (instruction as)
    (list-instruction "save/stores" instruction)
    (let* ((n  (operand1 instruction))
           (rs (operand2 instruction))
           (ss (operand3 instruction))
           (v  (make-vector (+ n 1) #f)))
      (do ((ss ss (cdr ss)))
          ((null? ss))
        (vector-set! v (car ss) #t))
      (emit-save0 as n)
      (do ((i 0 (+ i 1)))
          ((> i n))
        (if (not (vector-ref v i))
            (cant.sti as $r.tmp $r.stkp (fence.frame-slot i))))
      (do ((rs rs (cdr rs))
           (ss ss (cdr ss)))
          ((null? rs))
        (cant.sti as (car rs) $r.stkp (fence.frame-slot (car ss)))))))

(define-instruction $reg/op1/setreg     ; op rs rd
  (lambda (instruction as)
    (list-instruction "reg/op1/setreg" instruction)
    (emit-op1 as (operand1 instruction) (operand2 instruction) (operand3 instruction))))

(define-instruction $reg/op2/setreg     ; op rs1 rs2 rd
  (lambda (instruction as)
    (list-instruction "reg/op2/setreg" instruction)
    (emit-op2 as (operand1 instruction) (operand2 instruction) (operand3 instruction) (operand4 instruction))))

(define-instruction $reg/op2imm/setreg  ; op rs imm rd
  (lambda (instruction as)
    (list-instruction "reg/op2imm/setreg" instruction)
    (emit-op2imm as (operand1 instruction) (operand2 instruction) (operand3 instruction) (operand4 instruction))))

(define-instruction $reg/op3            ; op rs1 rs2 rs3
  (lambda (instruction as)
    (list-instruction "reg/op3" instruction)
    (emit-op3 as (operand1 instruction) (operand2 instruction) (operand3 instruction) (operand4 instruction))))

(define-instruction $reg/branchf        ; rs label
  (lambda (instruction as)
    (list-instruction "reg/branchf" instruction)
    (emit-branchf as (operand1 instruction) (make-asm-label as (operand2 instruction)))))

(define-instruction $reg/op1/branchf    ; op rs label
  (lambda (instruction as)
    (list-instruction "reg/op1/branchf" instruction)
    (emit-op1-branchf as 
                      (operand1 instruction)
                      (operand2 instruction)
                      (make-asm-label as (operand3 instruction)))))

(define-instruction $reg/op2/branchf    ; op rs1 rs2 label
  (lambda (instruction as)
    (list-instruction "reg/op2/branchf" instruction)
    (emit-op2-branchf as
                      (operand1 instruction)
                      (operand2 instruction) 
                      (operand3 instruction)
                      (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/op2imm/branchf ; op rs imm label
  (lambda (instruction as)
    (list-instruction "reg/op2imm/branchf" instruction)
    (emit-op2imm-branchf as
                         (operand1 instruction)
                         (operand2 instruction)
                         (operand3 instruction)
                         (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/check          ; rs label liveregs
  (lambda (instruction as)
    (list-instruction "reg/branchf" instruction)
    (emit-check as (operand1 instruction) (make-asm-label as (operand2 instruction)))))

(define-instruction $reg/op1/check      ; op rs label liveregs
  (lambda (instruction as)
    (list-instruction "reg/op1/check" instruction)
    (emit-op1-check as
                    (operand1 instruction)
                    (operand2 instruction)
                    (make-asm-label as (operand3 instruction)))))

(define-instruction $reg/op2/check      ; op rs1 rs2 label liveregs
  (lambda (instruction as)
    (list-instruction "reg/op2/check" instruction)
    (emit-op2-check as
                    (operand1 instruction)
                    (operand2 instruction) 
                    (operand3 instruction) 
                    (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/op2imm/check   ; op rs imm label liveregs
  (lambda (instruction as)
    (list-instruction "reg/op2imm/check" instruction)
    (emit-op2imm-check as
                       (operand1 instruction)
                       (operand2 instruction)
                       (operand3 instruction)
                       (make-asm-label as (operand4 instruction)))))

; For setrtn/invoke, setrtn/branch, and setrtn/jump the return point
; is the following instruction, which must be aligned - otherwise the
; return address will confuse the garbage collector.  We hide the
; alignment complexity within the Cant back-end.

(define-instruction $setrtn/invoke      ; alignment n
  (lambda (instruction as)
    (list-instruction "setrtn/invoke" instruction)
    (let ((a (operand1 instruction))
          (n (operand2 instruction)))
    (emit-invoke-setup as $r.result n $m.invoke-ex '*invoke-ex)
    (cant.setrtn/strtn/jump as #f a $stk.retaddr))))

(define-instruction $setrtn/branch      ; alignment L k
  (lambda (instruction as)
    (list-instruction "setrtn/branch" instruction)
    (let ((a (operand1 instruction))
          (L (make-asm-label as (operand2 instruction)))
          (k (operand3 instruction)))
      (if (label-value as L)
          (emit-timer-tick as))         ; Backward branch
      (cant.setrtn/strtn/branch as L a $stk.retaddr))))

(define-instruction $setrtn/jump        ; alignment m L
  (lambda (instruction as)
    (list-instruction "setrtn/jump" instruction)
    (let ((a (operand1 instruction))
          (m (operand2 instruction))
          (L (make-asm-label as (operand3 instruction))))
      (emit-timer-tick as)
      (emit-follow-lexical-chain as m $r.reg0)
      (cant.setrtn/strtn/jump as L a $stk.retaddr))))

(define-instruction $global/invoke      ; name n
  (lambda (instruction as)
    (list-instruction "global/invoke" instruction)
    (emit-global->register as (operand1 instruction) $r.second) ; exn handler wants the cell in SECOND
    (cant.ldi              as $r.second (- $tag.pair-tag) $r.result)
    (emit-invoke           as $r.result (operand2 instruction) $m.global-invoke-ex '*global-invoke-ex)))

(define-instruction $global/setrtn/invoke ; name alignment n
  (lambda (instruction as)
    (list-instruction "global/setrtn/invoke" instruction)
    (emit-global->register  as (operand1 instruction) $r.second) ; exn handler wants the cell in SECOND
    (cant.ldi               as $r.second (- $tag.pair-tag) $r.result)
    (emit-invoke-setup      as $r.result (operand3 instruction) $m.global-invoke-ex '*global-invoke-ex)
    (cant.setrtn/strtn/jump as #f (operand2 instruction) $stk.retaddr)))

; Procedures that are shared among multiple emitters because of
; peephole optimization.

(define (emit-invoke as rs nargs ex exname)
  (emit-invoke-setup as rs nargs ex exname)
  (cant.jump as #f))

(define (emit-branchf as rs L)          ; L has been converted with make-asm-label
  (if (label-value as L)
      (emit-timer-tick as))             ; Backward branch
  (cant.bcci as 'equal rs $imm.false L))

; Everything but the CTI

(define (emit-invoke-setup as rs nargs ex exname)
  (let ((L1 (fence.make-label as)))
    (if (runtime-safety-checking)
        (begin
          (cant.bcci as 'low3-equal $r.result $tag.procedure-tag L1)
          (if (not (= rs $r.result))
              (cant.mov as rs $r.result))
          (cant.trap as ex exname)
          (fence.label as L1)))
    (emit-timer-tick as)
    (cant.mov as $r.reg0 $r.second)
    (cant.mov as rs $r.reg0)
    (cant.movi as (fence.native->fixnum nargs) $r.result)))

(define (emit-check as r L)
  (if (runtime-safety-checking)
      (cant.bcci as 'equal r $imm.false L)))

; Allocate the frame and initialize its header fields.  The
; dynamic link field is not initialized; it is only used when the
; frame is flushed to the heap, never when the frame is in the
; stack (the size field serves the same purpose in the stack).

(define (emit-save0 as n)
  (let* ((L1        (fence.make-label as))
         (L0        (fence.make-label as))
         (framesize (* $bytewidth.wordsize (+ n 3))) ; Retaddr + dynamic link + REG0..REGn
         (realsize  (fence.roundup8 (+ framesize $bytewidth.wordsize)))) ; Additional word for the size field
    (fence.label as L0)
    (cant.ldi    as $r.globals $g.etop $r.tmp) ; System invariant: nursery top is stack limit (TODO: sce_buffer)
    ;; TODO: Should just use subi here (and the ARM back-end should implement subi as well as addi)
    (cant.addi   as $r.stkp (- realsize) $r.stkp)
    ;; Note order of arguments.
    ;; TODO: unsigned-less-or-equal would have been more appropriate but is not currently supported.
    (cant.bcc    as 'unsigned-less $r.tmp $r.stkp L1)
    (cant.addi   as $r.stkp realsize $r.stkp)
    (cant.trap   as $m.stkoflow '*stkoflow)
    (cant.ba     as L0)
    (fence.label as L1)
    (cant.movi   as framesize $r.tmp)
    (cant.sti    as $r.tmp $r.stkp $stk.contsize)
    (cant.movi   as 0 $r.tmp)                    ; $r.tmp == 0 is used by emit-save1 also
    (cant.sti    as $r.tmp $r.stkp $stk.dynlink) ; Clearing the dynamic link is redundant.  TODO: remove.
    (cant.sti    as $r.tmp $r.stkp $stk.retaddr)
    ;; Clearing the pad word is redundant, the GC does not inspect it.  TODO: remove.
    (if (and (= 4 $bytewidth.wordsize) 
             (= (- realsize framesize) 8))
        (cant.sti as $r.tmp $r.stkp (fence.frame-slot (+ n 1))))))

; Clear all the slots in the frame.  We depend on $r.tmp being
; set to zero when we enter.  (In principle we could reinitialize
; and count on the Cant back-end to optimize out the redundant
; initialization, if it is redundant.)

(define (emit-save1 as slots)
  (do ((i 0 (+ i 1))
       (offset $stk.reg0 (+ offset $bytewidth.wordsize)))
      ((> i slots))
    (cant.sti as $r.tmp $r.stkp offset)))

; emit-getglbl may destroy RESULT (because of the error trap).
; Note that rd may be RESULT or SECOND.

(define (emit-getglbl as name rd)
  (emit-global->register as name $r.tmp)
  (cant.ldi              as $r.tmp (- $tag.pair-tag) rd)
  (if (and (runtime-safety-checking)
           (catch-undefined-globals))
      (let ((L (fence.make-label as)))
        (cant.bcci      as 'not-equal rd $imm.undefined L)
        (cant.mov       as $r.tmp $r.result)
        (cant.trap      as $m.global-ex '*global-ex)
        (fence.label    as L))))

; emit-setglbl destroys RESULT (because of the write barrier).  Note
; that rs may be RESULT or SECOND.

(define (emit-setglbl as rs name)
  (if (not (= rs $r.second))
      (cant.mov          as rs $r.second))
  (emit-global->register as name $r.result)
  (cant.sti              as $r.second $r.result (- $tag.pair-tag))
  (emit-write-barrier    as $r.result $r.second))

    
;;; Helpers

; Byte offset within the stack frame of numbered slot

(define (fence.frame-slot slot)
  (+ (* $bytewidth.wordsize slot) $stk.reg0))

; Byte offset within the procedure of numbered slot, taking the
; tag into account as well.

(define (fence.procedure-slot slot)
  (+ (* $bytewidth.wordsize slot) $p.reg0))

; Offset within a constant vector of a numbered slot, taking the
; tag into account as well.

(define (fence.constvector-slot slot)
  (+ (* $bytewidth.wordsize slot) (- $bytewidth.wordsize $tag.vector-tag)))

; Conversion to/from fixnum

(define (fence.native->fixnum n) (* n 4))
(define (fence.fixnum->native n) (quotient n 4))

; Fixnum representations.  Arguably these belong in layouts.cfg.

; Most negative fixnum as a native number

(define fence.smallest-fixnum
  (case $bytewidth.wordsize
    ((4) (- (expt 2 29)))
    ((8) (- (expt 2 61)))
    (else ???)))

; Most positive fixnum as a native number

(define fence.largest-fixnum
  (case $bytewidth.wordsize
    ((4) (- (expt 2 29) 1))
    ((8) (- (expt 2 61) 1))
    (else ???)))

; Make a fresh label that's not confusable with MAL labels

(define (fence.make-label as)
  (make-asm-label as (new-label)))

; Is L something returned by fence.make-label?

(define (fence.label? L)
  (and (pair? L)
       (pair? (car L))
       (negative? (caar L))))

; Emit a fresh label created by fence.make-label

(define (fence.label as L)
  (cant.label as L))

; Round a value up to next increment of 8, x must be a fixnum
; as must the rounded-up value.

(define (fence.roundup8 x)
  (fxlogand (+ x 7) (fxlognot 7)))

;;; Emitter utilities

; Given an arbitrary constant opd, generate code to load it into a
; register r.  Destroys only r.

(define (emit-constant->register as opd r)
  (cond ((and (integer? opd) 
              (exact? opd)
              (<= fence.smallest-fixnum opd fence.largest-fixnum))
         (cant.movi as (fence.native->fixnum opd) r))
        ((boolean? opd)
         (cant.movi as (if opd $imm.true $imm.false) r))
        ((equal? opd (eof-object))
         (cant.movi as $imm.eof r))
        ((equal? opd (unspecified))
         (cant.movi as $imm.unspecified r))
        ((equal? opd (undefined))
         (cant.movi as $imm.undefined r))
        ((null? opd)
         (cant.movi as $imm.null r))
        ((char? opd)
         (cant.movi as (char-representation opd) r))
        (else
         (emit-load-constant as (emit-datum as opd) r))))

(define (char-representation c)
  (+ (* (char->integer c) 256) $imm.character))

; Given the name of a global, generate code that loads a pointer to its cell
; into a register r.  Destroys only r.

(define (emit-global->register as global r)
  (emit-load-constant as (emit-global as global) r))

; Load from the constant vector of the procedure in REG0 into r.
; Destroys only r.

(define (emit-load-constant as slot r)
  (cant.ldi as $r.reg0 $p.constvector r)
  (cant.ldi as r (fence.constvector-slot slot) r))

; Allocate and tag a procedure for n register slots (not counting reg0).
; Leaves the procedure in RESULT.  Destroys RESULT and TMP.

(define (emit-allocate-procedure as n)
  ;; There are four words of overhead in addition to the requested slots:
  ;;  - object header
  ;;  - code pointer
  ;;  - constant pointer
  ;;  - reg0 / dynamic link
  (emit-allocate-uninitialized as (* $bytewidth.wordsize (+ n 4))) ; Untagged value in RESULT
  (cant.addi as $r.result $tag.procedure-tag $r.result)
  (cant.movi as (+ (* (* (+ n 3) $bytewidth.wordsize) 256) $imm.procedure-header) $r.tmp)
  (cant.sti  as $r.tmp $r.result (- $tag.procedure-tag)))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'. If there are more data slots than registers, then
; we must generate code to cdr down the list in the last register to obtain
; the rest of the data. The list is expected to have at least the minimal
; length.
;
; The tagged pointer to the procedure is in $r.result.  Note $p.reg0
; accounts for the tag as well as header and non-saved-register
; content.

(define (emit-init-procedure-slots as n)

  (define (save-registers lo hi offset)
    (do ((lo     lo     (+ lo 1))
         (offset offset (+ offset $bytewidth.wordsize)))
        ((> lo hi))
      (cant.sti as lo $r.result offset)))

  (define (save-list lo hi offset)
    (cant.mov as *lastreg* $r.tmp)
    (do ((lo     lo     (+ lo 1))
         (offset offset (+ offset $bytewidth.wordsize)))
        ((> lo hi))
      (cant.ldi as $r.tmp (- $tag.pair-tag) $r.second)
      (cant.sti as $r.second $r.result offset)
      (if (< lo hi)
          (cant.ldi as $r.tmp (- $bytewidth.wordsize $tag.pair-tag) $r.tmp))))
      
  (cond ((< n *lastreg*)
         (save-registers 0 n $p.reg0))
        (else
         (save-registers 0 (- *lastreg* 1) $p.reg0)
         (save-list      *lastreg* n (+ $p.reg0 (* *lastreg* $bytewidth.wordsize))))))

; Signal a noncontinuable exception.

(define (emit-exception as exn r0 . rest)
  (let ((r1 (if (null? rest) $r.second (car rest))))
    (assert (not (and (= r0 $r.second) (= r1 $r.result)))) ; TODO: relax this restriction?
    (if (not (= r0 $r.result))
        (cant.mov as r0 $r.result))
    (if (not (= r1 $r.second))
        (cant.mov as r1 $r.second))
    (cant.trap as $m.exception '*exception exn)))

; Follow m links of the lexical chain and return a register
; pointing to that rib.  m may be zero.  Returns REG0 if m=0,
; otherwise 'target'.

(define (emit-follow-lexical-chain as m target)
  (let loop ((q m) (base $r.reg0))
    (if (zero? q)
        base
        (begin
          (cant.ldi as base $p.linkoffset target)
          (loop (- q 1) target)))))

; val has been stored in obj, now record that store.
; This may destroy RESULT and SECOND, and it will destroy TMP 
; only if r1=SECOND and r2=RESULT.
;
; TODO: Need to support full-barrier.
;
; To reduce code size we could imagine several entry points, eg, one
; for RESULT/SECOND, another for SECOND/RESULT.  Not clear how big the
; benefit would be.

(define (emit-write-barrier as r_obj r_val)

  ;; Not yet useful outside emit-write-barrier.
  (define (emit-millicode-arguments2 as r1 r2)
    (if (and (= r1 $r.second) (= r2 $r.result)) ; Must swap
        (begin
          (cant.mov as $r.result $r.tmp)
          (cant.mov as $r.second $r.result)
          (cant.mov as $r.tmp $r.second))
        (begin
          (if (not (= r1 $r.result))
              (cant.mov as r1 $r.result))
          (if (not (= r2 $r.second))
              (cant.mov as r2 $r.second)))))

  (let ((L (fence.make-label as)))
    (cant.bcci as 'low1-equal r_val 0 L)        ; Skip if value is not pointer
    (emit-millicode-arguments2 as r_obj r_val)
    (cant.trap as $m.partial-barrier '*partial-barrier)
    (fence.label as L)))

; Trap if the timer is zero.  Otherwise decrement it.

(define (emit-timer-tick as)
  (let ((L1 (fence.make-label as))
        (L2 (fence.make-label as)))
    (cant.ldi    as $r.globals $g.timer $r.tmp)
    (cant.bcci   as 'not-equal $r.tmp 0 L1)
    (cant.trap   as $m.timer-exception '*timer-exception) ; Kills $r.tmp
    (cant.ba     as L2)
    (fence.label as L1)
    (cant.addi   as $r.tmp -1 $r.tmp)
    (cant.sti    as $r.tmp $r.globals $g.timer)
    (fence.label as L2)))

; Allocate uninitialized memory and leave an untagged pointer in RESULT.
; nbytes must be divisible by the word size.
; This function must not destroy alloctmp, that is reserved for the caller.
; Destroys RESULT, obviously.
;
; We count on alloc checking implicitly against header overflow by
; guaranteeing that no allocation is so large that the length field
; would not fit in bits 8..30 of the header.
;
; TODO: Verify that alloc performs the necessary check.

(define (emit-allocate-uninitialized as nbytes)
  (if (not (zero? (remainder nbytes $bytewidth.wordsize)))
      (error "Allocation request size must be divisible by the word size"))
  (cant.movi as nbytes $r.result)
  (cant.trap as $m.alloc '*alloc))

; rinit is a register name or ().  If () we initialize with $imm.null,
; otherwise with the register contents.
;
; We count on alloci checking implicitly against header overflow by
; guaranteeing that no allocation is so large that the length field
; would not fit in bits 8..30 of the header.
;
; TODO: Verify that alloci performs the necessary check.

(define (emit-make-word-structure as rlen rinit rd tag hdr exn)
  (assert (or (null? rinit) (not (= rinit $r.result))))
  (if (= $bytewidth.wordsize 8)
      (error "emit-make-word-structure must correctly compute and store 64-bit length value")) ; FIXME64
  (emit-check-positive-fixnum as rlen exn)
  (cant.sti    as rlen $r.globals $g.alloctmp)             ; Save the actual length
  (cant.addi   as rlen (fence.native->fixnum 1) $r.result) ; Add one word for the header
  (if (null? rinit)
      (cant.movi as $imm.null $r.second)
      (cant.mov  as rinit $r.second))
  (cant.trap   as $m.alloci '*alloci)
  (cant.ldi    as $r.globals $g.alloctmp $r.tmp)
  (cant.shli   as $r.tmp 8 $r.tmp)
  (cant.addi   as $r.tmp hdr $r.tmp)
  (cant.sti    as $r.tmp $r.result 0)
  (cant.addi   as $r.result tag rd))

; We count on alloc-bv checking implicitly against header overflow by
; guaranteeing that no allocation is so large that the length field
; would not fit in bits 8..30 of the header.
;
; TODO: Verify that alloc-bv performs the necessary check.

(define (emit-make-byte-structure as rlen rd tag hdr exn)
  (if (= $bytewidth.wordsize 8)
      (error "emit-make-byte-structure must correctly compute and store 64-bit length value")) ; FIXME64
  (emit-check-positive-fixnum as rlen exn)
  (cant.sti    as rlen $r.globals $g.alloctmp)  ; Save the actual length
  (cant.addi   as rlen (fence.native->fixnum $bytewidth.wordsize) $r.result) ; Add space for header
  (cant.trap   as $m.alloc-bv '*alloc-bv)       ; Too complicated to inline
  (cant.ldi    as $r.globals $g.alloctmp $r.tmp); Load requested length
  (cant.shli   as $r.tmp 6 $r.tmp)              ;   insert into header
  (cant.addi   as $r.tmp hdr $r.tmp)            ;     with header tag
  (cant.sti    as $r.tmp $r.result 0)           ;       and store
  (cant.addi   as $r.result tag rd))            ; Tag

(define (emit-op1 as name rs rd)

  (define (trap0 code)
    (cant.trap as code name)
    (if (not (= rd $r.result))
        (cant.mov as $r.result rd)))

  (define (trap1 code)
    (if (not (= rs $r.result))
        (cant.mov as rs $r.result))
    (cant.trap as code name)
    (if (not (= rd $r.result))
        (cant.mov as $r.result rd)))

  (define (trap1/nr code)
    (if (not (= rs $r.result))
        (cant.mov as rs $r.result))
    (cant.trap as code name))

  (assert (not (= rs $r.second)))
  (case name
    ;; Constants - rs ignored
    ((unspecified)
     (cant.movi as $imm.unspecified rd))
    ((undefined)
     (cant.movi as $imm.undefined rd))
    ((eof-object)
     (cant.movi as $imm.eof rd))
    ((most-positive-fixnum)
     (cant.movi as (fence.native->fixnum fence.largest-fixnum) rd))
    ((most-negative-fixnum)
     (cant.movi as (fence.native->fixnum fence.smallest-fixnum) rd))

    ;; Type discrimination
    ((fixnum?)
     (cant.setcci as 'low2-equal rs 0 rd))
    ((char?)
     (cant.setcci as 'low8-equal rs $imm.character rd))
    ((null?)
     (cant.setcci as 'equal rs $imm.null rd))
    ((eof-object?)
     (cant.setcci as 'equal rs $imm.eof rd))
    ((pair?)
     (cant.setcci as 'low3-equal rs $tag.pair-tag rd))
    ((bytevector-like?)
     (cant.setcci as 'low3-equal rs $tag.bytevector-tag rd))
    ((bytevector?)
     (emit-test-tags as rs rd $tag.bytevector-tag $hdr.bytevector))
    ((flonum?)
     (emit-test-tags as rs rd $tag.bytevector-tag $hdr.flonum))
    ((compnum?)
     (emit-test-tags as rs rd $tag.bytevector-tag $hdr.compnum))
    ((vector-like?)
     (cant.setcci as 'low3-equal rs $tag.vector-tag rd))
    ((vector?)
     (emit-test-tags as rs rd $tag.vector-tag $hdr.vector))
    ((symbol?)
     (emit-test-tags as rs rd $tag.vector-tag $hdr.symbol))
    ((port?)
     (emit-test-tags as rs rd $tag.vector-tag $hdr.port))
    ((structure?)
     (emit-test-tags as rs rd $tag.vector-tag $hdr.struct))
    ((procedure?)
     (cant.setcci as 'low3-equal rs $tag.procedure-tag rd))

    ;; Pairs and cells
    ((car:pair cell-ref)
     (cant.ldi as rs (- $tag.pair-tag) rd))
    ((cdr:pair)
     (cant.ldi as rs (- $bytewidth.wordsize $tag.pair-tag) rd))
    ((car)
     (emit-check-tag as rs $tag.pair-tag $ex.car)
     (cant.ldi as rs (- $tag.pair-tag) rd))
    ((cdr)
     (emit-check-tag as rs $tag.pair-tag $ex.cdr)
     (cant.ldi as rs (- $bytewidth.wordsize $tag.pair-tag) rd))
    ((make-cell)
     (if (= rs $r.result)
         (cant.sti as $r.result $r.globals $g.alloctmp))
     (emit-allocate-uninitialized as (* 2 $bytewidth.wordsize)) ; Clobbers RESULT
     (if (= rs $r.result)
         (begin (cant.ldi as $r.globals $g.alloctmp $r.tmp)
                (cant.sti as $r.tmp $r.result 0))
         (cant.sti as rs $r.result 0))
     (cant.movi as $imm.null $r.second)
     (cant.sti  as $r.second $r.result $bytewidth.wordsize)
     ;; This is always OK; the value in RESULT looks like a fixnum.
     (cant.addi as $r.result $tag.pair-tag rd))

    ;; Vectors
    ((vector-length:vec)
     (cant.ldi as rs (- $tag.vector-tag) rd)
     (cant.shri as rd 8 rd))
    ((vector-length)
     (emit-structure-length2 as rs rd $tag.vector-tag $hdr.vector $ex.vref))
    ((vector-like-length)
     (emit-structure-length1 as rs rd $tag.vector-tag $ex.vllen))

    ;; Procedures
    ((make-procedure)
     (emit-make-word-structure as rs '() rd $tag.procedure-tag $hdr.procedure $ex.mkvl))
    ((procedure-length)
     (emit-structure-length1 as rs rd $tag.procedure-tag $ex.plen))

    ;; Bytevectors
    ((make-bytevector)
     (emit-make-byte-structure as rs rd $tag.bytevector-tag $hdr.bytevector $ex.mkbvl))
    ((bytevector-like-length:bvl)
     (cant.ldi as rs (- $tag.bytevector-tag) rd)
     (cant.shri as rd 6 rd)
     (cant.andni as rd 3 rd))
    ((bytevector-length)
     (emit-check-tags as rs $tag.bytevector-tag $hdr.bytevector $ex.bvlen)
     (emit-op1 as 'bytevector-like-length:bvl rs rd))
    ((bytevector-like-length)
     (emit-check-tag as rs $tag.bytevector-tag $ex.bvllen)
     (emit-op1 as 'bytevector-like-length:bvl rs rd))

    ;; Strings
    ((ustring?)
     (case (nbuild-parameter 'target-string-rep)
       ((flat1)
        (emit-test-tags as rs rd $tag.bytevector-tag $hdr.string))
       ((flat4)
        (emit-test-tags as rs rd $tag.bytevector-tag (+ $imm.bytevector-header $tag.ustring-typetag)))
       (else
        (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))
    ((ustring-length:str) 
     (case (nbuild-parameter 'target-string-rep)
       ((flat1)
        (emit-op1 as 'bytevector-like-length:bvl rs rd))
       ((flat4)
        (cant.ldi as rs (- $tag.bytevector-tag) rd)
        (cant.shri as rd 8 rd))
       (else
        (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))

    ;; Characters.  Note the two high bits of the low byte, as well as
    ;; the high bit of the word, are zero, so no masking is needed
    ;; when converting a character to a fixnum,
    ((char->integer)
     (emit-check-character as rs $ex.char2int)
     (cant.shri as rs 6 rd))
    ((char->integer:chr)
     (cant.shri as rs 6 rd))
    ((integer->char:trusted)
     (cant.shli as rs 6 rd)
     (cant.addi as rd $imm.character rd))
    ((integer->char:fix)
     (if (runtime-safety-checking)
         (let ((L0 (fence.make-label as))
               (L1 (fence.make-label as)))
           ;; Argument cannot be a surrogate (#x0000d800 - #x0000dfff).
           (cant.shri as rs 13 $r.tmp) ; 8+3 bits, plus 2 for the tag, leaving the high bit of the third nibble and everything above
           (cant.bcci as 'not-equal $r.tmp #b11011 L1) ; #b11011 = #xd800 >> 11
           (fence.label as L0)
           (emit-exception as $ex.int2char rs)
           (fence.label as L1)
           ;; Argument must be non-negative and less than #x00110000.
           (cant.bcci as 'greater-or-equal $r.tmp 544 L0) ; 544 = #x110000 >> 11
           (cant.bcci as 'less $r.tmp 0 L0))) ; TODO: merge into the previous line with the unsigned trick?
     (cant.shli as rs 6 rd)
     (cant.addi as rd $imm.character rd))
    ((integer->char)
     (emit-check-fixnum as rs $ex.int2char)
     (emit-op1 as 'integer->char:fix rs rd))

    ;; Numbers
    ((--)                               ; TODO: fixnum fast case?
     (trap1 $m.negate))
    ((fx--)
     (if (runtime-safety-checking)
         (let ((L (fence.make-label as)))
           (emit-check-fixnum as rs $ex.fx--)
           (cant.movi as 0 $r.tmp)
           (cant.sub/bnv  as $r.tmp rs rd L)
           (emit-exception as $ex.fx-- rs)
           (fence.label as L))
         (begin
           (cant.movi as 0 $r.tmp)
           (cant.sub/bnv  as $r.tmp rs rd L))))
    ((fxzero?)
     (emit-check-fixnum as rs $ex.fxzero?)
     (cant.setcci as 'equal rs 0 rd))
    ((fxpositive?)
     (emit-check-fixnum as rs $ex.fxpositive?)
     (cant.setcci as 'greater-or-equal rs 0 rd))
    ((fxnegative?)
     (emit-check-fixnum as rs $ex.fxnegative?)
     (cant.setcci as 'less rs 0 rd))
    ((fxlognot)
     (emit-check-fixnum as rs $ex.lognot)
     (cant.not as rs rd)
     (cant.andni as rd 3 rd))
    ((abs)                              ; TODO: fixnum fast case?
     (trap1 $m.abs))

    ;; Boolean
    ((not)
     (cant.setcci as 'equal rs $imm.false rd))

    ;; Odds and ends
    ((gc-counter)
     (cant.ldi as $r.globals $g.gccnt rd))
    ((major-gc-counter)
     (cant.ldi as $r.globals $g.majorgccnt rd))
    ((machine-address)
     (cant.andni as rs #b1111 rd)
     (cant.shri as rd 2 rd))

    ;; Miscellaneous operations that call millicode.  In all cases
    ;; these are allowed to destroy RESULT and SECOND.
    ((syscall)
     (trap1 $m.syscall))
    ((creg)
     (trap0 $m.creg))
    ((creg-set!)
     (trap1/nr $m.creg-set!))
    ((break)                            ; Ignore rs /and/ rd
     (cant.trap as $m.break name))
    ((enable-interrupts)
     (trap1/nr $m.enable-interrupts))
    ((disable-interrupts)
     (trap0 $m.disable-interrupts))
    ((typetag)
     (trap1 $m.typetag))
    ((exact->inexact)
     (trap1 $m.exact->inexact))
    ((inexact->exact)
     (trap1 $m.inexact->exact))
    ((real-part)
     (trap1 $m.real-part))
    ((imag-part)
     (trap1 $m.imag-part))
    ((round)
     (trap1 $m.round))
    ((truncate)
     (trap1 $m.truncate))
    ((zero?)                            ; TODO: fixnum fast case?
     (trap1 $m.zerop))
    ((complex?)
     (trap1 $m.complexp))
    ((rational?)
     (trap1 $m.rationalp))
    ((integer?)                         ; TODO: fixnum fast case?
     (trap1 $m.integerp))
    ((exact?)                           ; TODO: fixnum fast case?
     (trap1 $m.exactp))
    ((inexact?)                         ; TODO: fixnum fast case?
     (trap1 $m.inexactp))

    (else
     (error "Unknown 1-argument primitive: " name))))

(define (emit-op1-branchf as name rs label)
  ;; TODO: clean up when we clean up zero?
  (if (not (eq? name 'zero?))
      (if (label-value as label)
          (emit-timer-tick as)))
  (case name
    ((null?)
     (cant.bcci as 'not-equal rs $imm.null label))
    ((eof-object?)
     (cant.bcci as 'not-equal rs $imm.eof label))
    ((pair?)
     (cant.bcci as 'low3-not-equal rs $tag.pair-tag label))
    ((zero?)
     ;; TODO: implement better?
     (emit-op1 as name rs $r.result)
     (emit-branchf as $r.result label))
    ((fixnum?)
     (cant.bcci as 'low2-not-equal rs 0 label))
    ((flonum?)
     (emit-test-tags-branchf as rs label $tag.bytevector-tag $hdr.flonum))
    ((vector?)
     (emit-test-tags-branchf as rs label $tag.vector-tag $hdr.vector))
    ((bytevector?)
     (emit-test-tags-branchf as rs label $tag.bytevector-tag $hdr.bytevector))
    ((structure?)
     (emit-test-tags-branchf as rs label $tag.vector-tag $hdr.struct))
    (else
     (error "Unknown op1-branchf primitive: " name))))

(define (emit-op1-check as name rs label)
  (if (runtime-safety-checking)
      (case name
        ((fixnum?)
         (cant.bcci as 'low2-not-equal rs 0 label))
        ((flonum?)
         (emit-test-tags-branchf as rs label $tag.bytevector-tag $hdr.flonum))
        ((pair?)
         (cant.bcci as 'low3-not-equal rs $tag.pair-tag label))
        ((vector?)
         (emit-test-tags-branchf as rs label $tag.vector-tag $hdr.vector))
        ((bytevector?)
         (emit-test-tags-branchf as rs label $tag.bytevector-tag $hdr.bytevector))
        ((structure?)
         (emit-test-tags-branchf as rs label $tag.vector-tag $hdr.struct))
        ((ustring?)
         (case (nbuild-parameter 'target-string-rep)
           ((flat1)
            (emit-test-tags-branchf as rs label $tag.bytevector-tag $hdr.string))
           ((flat4)
            (emit-test-tags-branchf as rs label $tag.bytevector-tag (+ $imm.bytevector-header $tag.ustring-typetag)))
           (else
            (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))
        (else
         (error "Unknown op1-check primitive: " name)))))

(define (emit-op2 as name rs1 rs2 rd)

  (define (trap2 code)
    (if (not (= rs1 $r.result))
        (cant.mov as rs1 $r.result))
    (if (not (= rs2 $r.second))
        (cant.mov as rs2 $r.second))
    (cant.trap as code name)
    (if (not (= rd $r.result))
        (cant.mov as $r.result rd)))

  (define (trap2/nr code)
    (if (not (= rs1 $r.result))
        (cant.mov as rs1 $r.result))
    (if (not (= rs2 $r.second))
        (cant.mov as rs2 $r.second))
    (cant.trap as code name))

  (define (fixnum-fast-case op trap-code)
    (let ((L1 (fence.make-label as))
          (L2 (fence.make-label as)))
      (cant.or     as rs1 rs2 $r.tmp)
      (cant.bcci   as 'low2-not-equal $r.tmp 0 L1)
      (op L2)
      (fence.label as L1)
      (trap2 trap-code)
      (fence.label as L2)))

  (define (flonum-math op)
    (cant.fldi as rs1 (+ 8 (- $tag.bytevector-tag)) $r.f0)
    (cant.fldi as rs2 (+ 8 (- $tag.bytevector-tag)) $r.f1)
    (op        as $r.f0 $r.f1 $r.f0)
    (emit-allocate-uninitialized as 16)
    (cant.movi as (+ (* 16 256) $hdr.flonum) $r.tmp)
    (cant.sti  as $r.tmp $r.result 0)
    (cant.fsti as $r.f0 $r.result 8)
    (cant.addi as $r.result $tag.bytevector-tag rd))

  (define (flonum-compare op)
    (cant.fldi   as rs1 (+ 8 (- $tag.bytevector-tag)) $r.f0)
    (cant.fldi   as rs2 (+ 8 (- $tag.bytevector-tag)) $r.f1)
    (cant.fsetcc as op $r.f0 $r.f1 rd))

  (assert (not (= rs1 $r.second)))
  (assert (not (= rs2 $r.result)))
  (case name
    ((cell-set!)
     (cant.sti as rs2 rs1 (- $tag.pair-tag))
     (emit-write-barrier as rs1 rs2))
    ((cell-set!:nwb)
     (cant.sti as rs2 rs1 (- $tag.pair-tag)))
    ((set-car!)
     (emit-check-tag as rs1 $tag.pair-tag $ex.setcar)
     (cant.sti as rs2 rs1 (- $tag.pair-tag))
     (emit-write-barrier as rs1 rs2))
    ((set-cdr!)
     (emit-check-tag as rs1 $tag.pair-tag $ex.setcdr)
     (cant.sti as rs2 rs1 (- $bytewidth.wordsize $tag.pair-tag))
     (emit-write-barrier as rs1 rs2))
    ((cons)
     (let ((spill? (or (= rs1 $r.result) (= rs2 $r.result))))
       (if spill?
           (cant.sti as $r.result $r.globals $g.alloctmp))
       (emit-allocate-uninitialized as (* 2 $bytewidth.wordsize))
       (if spill?
           (cant.ldi as $r.globals $g.alloctmp $r.tmp))
       (if (= rs1 $r.result)
           (cant.sti as $r.tmp $r.result 0)
           (cant.sti as rs1 $r.result 0))
       (if (= rs2 $r.result)
           (cant.sti as $r.tmp $r.result $bytewidth.wordsize)
           (cant.sti as rs2 $r.result $bytewidth.wordsize))
       (cant.addi as $r.result $tag.pair-tag rd)))

    ;; Vectors
    ((vector-ref:trusted)
     (emit-unchecked-structure-ref as rs1 rs2 rd $tag.vector-tag))
    ((make-vector)
     (emit-make-word-structure as rs1 rs2 rd $tag.vector-tag $hdr.vector $ex.mkvl))
    ((vector-ref)
     (emit-structure-ref2 as rs1 rs2 rd $tag.vector-tag $hdr.vector $ex.vref))
    ((vector-like-ref)
     (emit-structure-ref1 as rs1 rs2 rd $tag.vector-tag $ex.vlref))

    ;; Procedures
    ((procedure-ref)
     (emit-structure-ref1 as rs1 rs2 rd $tag.procedure-tag $ex.pref))

    ;; Bytevectors
    ((bytevector-like-ref:trusted)
     (cant.shri  as rs2 2 $r.tmp)
     (cant.add   as rs1 $r.tmp rd)
     (cant.ldbiz as rd (- $bytewidth.wordsize $tag.bytevector-tag) rd)
     (cant.shli  as rd 2 rd))
    ((bytevector-ref)
     (emit-check-tags as rs1 $tag.bytevector-tag $hdr.bytevector $ex.bvref)
     (emit-check-fixnum as rs2 $ex.bvref)
     (emit-check-byte-range as $tag.bytevector-tag rs1 rs2 $ex.bvref)
     (emit-op2 as 'bytevector-like-ref:trusted rs1 rs2 rd))
    ((bytevector-like-ref)
     (emit-check-tag as rs1 $tag.bytevector-tag $ex.bvlref)
     (emit-check-fixnum rs1 $ex.bvlref)
     (emit-check-byte-range as $tag.bytevector-tag rs1 rs2 $ex.bvlref)
     (emit-op2 as 'bytevector-like-ref:trusted rs1 rs2 rd))
    ((bytevector-fill!)
     ;; The Sparc version has a double tag test here but the
     ;; Standard-C and IAssassin versions have only a single tag test,
     ;; and we fault in the current (July 2012) strings code if we
     ;; look too closely.
     (emit-check-tag as rs1 $tag.bytevector-tag $ex.bvfill)
     (trap2 $m.bytevector-like-fill))

    ;; Strings
    ((make-ustring)
     (case (nbuild-parameter 'target-string-rep)
       ((flat1)
        ;; TODO: Does rs2 need to be saved across the allocation call?
        ;; Beware that emit-make-byte-structure uses $g.alloctmp.
        (emit-check-character as rs2 $ex.mkstr)
        (emit-make-byte-structure as rs1 $r.result $tag.bytevector-tag $hdr.string)
        (cant.shri as rs2 6 $r.second)
        (cant.andni as $r.second 3 $r.second)
        (cant.trap as $m.bytevector-like-fill '*bytevector-like-fill)
        (cant.mov  as $r.result rd))
       ((flat4)
        (emit-check-character as rs2 $ex.mkstr)
        (if (= $bytewidth.wordsize 4)
            (emit-make-word-structure as rs1 rs2 rd $tag.bytevector-tag (+ $imm.bytevector-header $tag.ustring-typetag) $ex.mkstr)
            (error "make-ustring not implemented for 64-bit systems"))) ; FIXME64
       (else
        (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))
    ((ustring-ref:trusted)
     (case (nbuild-parameter 'target-string-rep)
       ((flat1)
        (cant.shri  as rs2 2 $r.tmp)
        (cant.add   as rs1 $r.tmp rd)
        (cant.ldbiz as rd (- $bytewidth.wordsize $tag.bytevector-tag) rd)
        (cant.shli  as rd 8 rd)
        (cant.addi  as rd $imm.character rd))
       ((flat4)
        (cant.add as rs1 rs2 rd)
        (if (= $bytewidth.wordsize 4)
            (cant.ldi as rd (- $bytewidth.wordsize $tag.bytevector-tag) rd)
            (error "ustring-ref:trusted not implemented for 64-bit systems"))) ; FIXME64
       (else
        (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))

    ;; Characters
    ((char=?)
     (emit-check-characters as rs1 rs2 $ex.char=?)
     (cant.setcc as 'equal rs1 rs2 rd))
    ((char>=?)
     (emit-check-characters as rs1 rs2 $ex.char>=?)
     (cant.setcc as 'greater-or-equal rs1 rs2 rd))
    ((char>?)
     (emit-check-characters as rs1 rs2 $ex.char>?)
     (cant.setcc as 'greater rs1 rs2 rd))
    ((char<=?)
     (emit-check-characters as rs1 rs2 $ex.char<=?)
     (cant.setcc as 'less-or-equal rs1 rs2 rd))
    ((char<?)
     (emit-check-characters as rs1 rs2 $ex.char<?)
     (cant.setcc as 'less rs1 rs2 rd))

    ;; Numbers
    ((+:idx:idx)
     (cant.add as rs1 rs2 rd))
    ((-:idx:idx)
     (cant.sub as rs1 rs2 rd))
    ((+:fix:fix)
     (let ((L1 (fence.make-label as)))
       (cant.add/bnv as rs1 rs2 rd L1)
       (trap2 $m.add)
       (fence.label as L1)))
    ((-:fix:fix)
     (let ((L1 (fence.make-label as)))
       (cant.sub/bnv    as rs1 rs2 rd L1)
       (trap2 $m.subtract)
       (fence.label as L1)))
    ((=:fix:fix)
     (cant.setcc as 'equal rs1 rs2 rd))
    ((<:fix:fix)
     (cant.setcc as 'less rs1 rs2 rd))
    ((<=:fix:fix)
     (cant.setcc as 'less-or-equal rs1 rs2 rd))
    ((>=:fix:fix)
     (cant.setcc as 'greater-or-equal rs1 rs2 rd))
    ((>:fix:fix)
     (cant.setcc as 'greater rs1 rs2 rd))
    ((+)
     (fixnum-fast-case (lambda (Ldone) 
                         (cant.add/bnv as rs1 rs2 rd Ldone))
                       $m.add))
    ((-)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.sub/bnv as rs1 rs2 rd Ldone))
                       $m.subtract))
    ((*)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.shrai as rs2 2 $r.tmp)
                         (cant.mul/bnv as rs1 $r.tmp rd Ldone))
                       $m.multiply))
    ((=)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.setcc as 'equal rs1 rs2 rd)
                         (cant.ba    as Ldone))
                       $m.numeq))
    ((<)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.setcc as 'less rs1 rs2 rd)
                         (cant.ba    as Ldone))
                       $m.numlt))
    ((<=)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.setcc as 'less-or-equal rs1 rs2 rd)
                         (cant.ba    as Ldone))
                       $m.numle))
    ((>)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.setcc as 'greater rs1 rs2 rd)
                         (cant.ba    as Ldone))
                       $m.numgt))
    ((>=)
     (fixnum-fast-case (lambda (Ldone)
                         (cant.setcc as 'greater-or-equal rs1 rs2 rd)
                         (cant.ba    as Ldone))
                       $m.numge))
    ((fx+)
     (if (runtime-safety-checking)
         (let ((L (fence.make-label as)))
           (emit-check-fixnums as rs1 rs2 $ex.fx+)
           (cant.add/bnv as rs1 rs2 rd L)
           (emit-exception as $ex.fx+ rs1 rs2)
           (fence.label as L))
         (cant.add as rs1 rs2 rd)))
    ((fx-)
     (if (runtime-safety-checking)
         (let ((L (fence.make-label as)))
           (emit-check-fixnums as rs1 rs2 $ex.fx-)
           (cant.sub/bnv as rs1 rs2 rd L)
           (emit-exception as $ex.fx- rs1 rs2)
           (fence.label as L))
         (cant.sub as rs1 rs2 rd)))
    ((fx*)
     (if (runtime-safety-checking)
         (let ((L (fence.make-label as)))
           (emit-check-fixnums as rs1 rs2 $ex.fx*)
           (cant.shrai as rs2 2 $r.tmp)
           (cant.mul/bnv as rs1 $r.tmp rd L)
           (emit-exception as $ex.fx* rs1 rs2)
           (fence.label as L))
         (begin
           (cant.shrai as rs2 2 $r.tmp)
           (cant.mul as rs1 $r.tmp rd))))
    ((fx=)
     (emit-check-fixnums as rs1 rs2 $ex.fx=)
     (cant.setcc as 'equal rs1 rs2 rd))
    ((fx<)
     (emit-check-fixnums as rs1 rs2 $ex.fx<)
     (cant.setcc as 'less rs1 rs2 rd))
    ((fx<=)
     (emit-check-fixnums as rs1 rs2 $ex.fx<=)
     (cant.setcc as 'less-or-equal rs1 rs2 rd))
    ((fx>)
     (emit-check-fixnums as rs1 rs2 $ex.fx>)
     (cant.setcc as 'greater rs1 rs2 rd))
    ((fx>=)
     (emit-check-fixnums as rs1 rs2 $ex.fx>=)
     (cant.setcc as 'greater-or-equal rs1 rs2 rd))
    ((fxlogand)
     (emit-check-fixnums as rs1 rs2 $ex.logand)
     (cant.and      as rs1 rs2 rd))
    ((fxlogior)
     (emit-check-fixnums as rs1 rs2 $ex.logior)
     (cant.or      as rs1 rs2 rd))
    ((fxlogxor)
     (emit-check-fixnums as rs1 rs2 $ex.logxor)
     (cant.xor      as rs1 rs2 rd))
    ((fxlsh)
     (emit-check-fixnums as rs1 rs2 $ex.lsh) ; TODO: require positive fixnum for op2?
     (cant.shri     as rs2 2 $r.tmp)
     (cant.shl      as rs1 $r.tmp rd))
    ((fxrshl)
     (emit-check-fixnums as rs1 rs2 $ex.rshl) ; TODO: require positive fixnum for op2?
     (cant.shri     as rs2 2 $r.tmp)
     (cant.shr      as rs1 $r.tmp rd)
     (cant.andni    as rd 3 rd))
    ((fxrsha)
     (emit-check-fixnums as rs1 rs2 $ex.rsha) ; TODO: require positive fixnum for op2?
     (cant.shri     as rs2 2 $r.tmp)
     (cant.shra     as rs1 $r.tmp rd)
     (cant.andni    as rd 3 rd))
    ((rot)
     (error "ROT is not implemented, was it ever?"))
    ((+:flo:flo)
     (flonum-math cant.fadd))
    ((-:flo:flo)
     (flonum-math cant.fsub))
    ((*:flo:flo)
     (flonum-math cant.fmul))
    ((/:flo:flo)
     (flonum-math cant.fdiv))
    ((=:flo:flo)
     (flonum-compare 'equal))
    ((>=:flo:flo)
     (flonum-compare 'greater-or-equal))
    ((>:flo:flo)
     (flonum-compare 'greater))
    ((<=:flo:flo)
     (flonum-compare 'less-or-equal))
    ((<:flo:flo)
     (flonum-compare 'less))

    ;; Misc
    ((eq?)
     (cant.setcc as 'equal rs1 rs2 rd))

    ;; Miscellaneous operations that call millicode.  In all cases
    ;; these are allowed to destroy RESULT and SECOND.
    ((typetag-set!)
     (trap2/nr $m.typetag-set))
    ((eqv?)
     (trap2 $m.eqv))
    ((sys$partial-list->vector)
     (trap2 $m.partial-list->vector))
    ((sys$bvlcmp)
     (trap2 $m.bvlcmp))
    ((/)
     (trap2 $m.divide))
    ((quotient)                         ; TODO: in-line fast path?
     (trap2 $m.quotient))
    ((remainder)                        ; TODO: in-line fast path?
     (trap2 $m.remainder))

    (else
     (error "Unknown 2-argument primitive: " name))))

(define (emit-op2-branchf as name rs1 rs2 label)

  (define (fixnum-fast-case op trap-code)
    (let ((L1 (fence.make-label as))
          (L2 (fence.make-label as)))
      (cant.or     as rs1 rs2 $r.tmp)
      (cant.bcci   as 'low2-not-equal $r.tmp 0 L1)
      (cant.bcc    as op rs1 rs2 label)
      (cant.ba     as L2)
      (fence.label as L1)
      (if (not (= rs1 $r.result))
          (cant.mov as rs1 $r.result))
      (if (not (= rs2 $r.second))
          (cant.mov as rs2 $r.second))
      (cant.trap   as trap-code name)
      (cant.bcci   as 'equal $r.result $imm.false label)
      (fence.label as L2)))

  (if (label-value as label)
      (emit-timer-tick as))
  (case name
    ((eq?)
     (cant.bcc as 'not-equal rs1 rs2 label))
    ((<)
     (fixnum-fast-case 'greater-or-equal $m.numlt))
    ((<=)
     (fixnum-fast-case 'greater $m.numle))
    ((=)
     (fixnum-fast-case 'not-equal $m.numeq))
    ((>)
     (fixnum-fast-case 'less-or-equal $m.numgt))
    ((>=)
     (fixnum-fast-case 'less $m.numge))
    ((<:fix:fix)
     (cant.bcc as 'greater-or-equal rs1 rs2 label))
    ((<=:fix:fix)
     (cant.bcc as 'greater rs1 rs2 label))
    ((=:fix:fix)
     (cant.bcc as 'not-equal rs1 rs2 label))
    ((>:fix:fix)
     (cant.bcc as 'less-or-equal rs1 rs2 label))
    ((>=:fix:fix)
     (cant.bcc as 'less rs1 rs2 label))
    (else
     (error 'emit-op2-branchf "Unknown: " name))))

(define (emit-op2-check as name rs1 rs2 label)
  (if (runtime-safety-checking)
      (case name
        ((<:fix:fix)
         (cant.bcc as 'greater-or-equal rs1 rs2 label))
        ((<=:fix:fix)
         (cant.bcc as 'greater rs1 rs2 label))
        ((=:fix:fix)
         (cant.bcc as 'not-equal rs1 rs2 label))
        ((>:fix:fix)
         (cant.bcc as 'less-or-equal rs1 rs2 label))
        ((>=:fix:fix)
         (cant.bcc as 'less rs1 rs2 label))
        (else
         (error 'emit-op2-check "Unknown: " name)))))

; TODO: "*" is commented-out in the primop table, why?
; TODO: "string-ref" is commented-out in the primop table, why?

(define (emit-op2imm as name rs imm rd)

  (define (trap2 imm code)
    (if (not (= rs $r.result))
        (cant.mov as rs $r.result))
    (cant.movi as imm $r.second)
    (cant.trap as code name)
    (if (not (= rd $r.result))
        (cant.mov as $r.result rd)))

  ;; First do non-fixnum cases, then fixnum cases

  (case name
    ((char<?)
     ;; A character constant is so large that the most meaningful
     ;; thing to do is to just load it into a register.
     (cant.movi as (char-representation imm) $r.second)
     (emit-check-character as rs $ex.char<?)
     (cant.setcc as 'less rs $r.second rd))
    ((char<=?)
     (cant.movi as (char-representation imm) $r.second)
     (emit-check-character as rs $ex.char<=?)
     (cant.setcc as 'less-or-equal rs $r.second rd))
    ((char=?)
     (cant.movi as (char-representation imm) $r.second)
     (emit-check-character as rs $ex.char=?)
     (cant.setcc as 'equal rs $r.second rd))
    ((char>?)
     (cant.movi as (char-representation imm) $r.second)
     (emit-check-character as rs $ex.char>?)
     (cant.setcc as 'greater rs $r.second rd))
    ((char>=?)
     (cant.movi as (char-representation imm) $r.second)
     (emit-check-character as rs $ex.char>=?)
     (cant.setcc as 'greater-or-equal rs $r.second rd))
    ((eq?)
     (cond ((eq? imm #t)
            (cant.setcci as 'equal rs $imm.true rd))
           ((eq? imm #f)
            (cant.setcci as 'equal rs $imm.false rd))
           ((eq? imm '())
            (cant.setcci as 'equal rs $imm.null rd))
           ((fence-imm? imm)
            (cant.setcci as 'equal rs (fence.native->fixnum imm) rd))
           (else
            (error 'emit-op2imm "Unhandled immediate for eq?:" imm))))
    (else
     (let ((imm (fence.native->fixnum imm)))

       (define (fixnum-fast-case op trap-code)
         (let ((L1 (fence.make-label as))
               (L2 (fence.make-label as)))
           (cant.bcci   as 'low2-not-equal rs 0 L1)
           (op L2)
           (fence.label as L1)
           (trap2 imm trap-code)
           (fence.label as L2)))

       (case name
         ((+:idx:idx)
          (cant.addi as rs imm rd))
         ((-:idx:idx)
          (cant.subi as rs imm rd))
         ((+:fix:fix)
          (let ((L1 (fence.make-label as)))
            (cant.addi/bnv as rs imm rd L1)
            (trap2 imm $m.add)
            (fence.label as L1)))
         ((-:fix:fix)
          (let ((L1 (fence.make-label as)))
            (cant.subi/bnv   as rs imm rd L1)
            (trap2 imm $m.subtract)
            (fence.label as L1)))
         ((=:fix:fix)
          (cant.setcci as 'equal rs imm rd))
         ((<:fix:fix)
          (cant.setcci as 'less rs imm rd))
         ((<=:fix:fix)
          (cant.setcci as 'less-or-equal rs imm rd))
         ((>=:fix:fix)
          (cant.setcci as 'greater-or-equal rs imm rd))
         ((>:fix:fix)
          (cant.setcci as 'greater rs imm rd))
         ((+)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.addi/bnv as rs imm rd Ldone))
                            $m.add))
         ((-)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.subi/bnv as rs imm rd Ldone))
                            $m.subtract))
         ((<)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.setcci as 'less rs imm rd)
                              (cant.ba     as Ldone))
                            $m.numlt))
         ((<=)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.setcci as 'less-or-equal rs imm rd)
                              (cant.ba     as Ldone))
                            $m.numle))
         ((=)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.setcci as 'equal rs imm rd)
                              (cant.ba     as Ldone))
                            $m.numeq))
         ((>)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.setcci as 'greater rs imm rd)
                              (cant.ba     as Ldone))
                            $m.numgt))
         ((>=)
          (fixnum-fast-case (lambda (Ldone)
                              (cant.setcci as 'greater-or-equal rs imm rd)
                              (cant.ba     as Ldone))
                            $m.numge))
         ((fx+)
          (if (runtime-safety-checking)
              (let ((L (fence.make-label as)))
                (emit-check-fixnum/imm as rs imm $ex.fx+)
                (cant.addi/bnv as rs imm rd L)
                (cant.movi as imm $r.second)
                (emit-exception as $ex.fx+ rs $r.second)
                (fence.label as L))
              (cant.addi as rs imm rd)))
         ((fx-)
          (if (runtime-safety-checking)
              (let ((L (fence.make-label as)))
                (emit-check-fixnum/imm as rs imm $ex.fx-)
                (cant.subi/bnv as rs imm rd L)
                (cant.movi as imm $r.second)
                (emit-exception as $ex.fx- rs $r.second)
                (fence.label as L))
              (cant.subi as rs imm rd)))
         ((fx=)
          (emit-check-fixnum/imm as rs imm $ex.fx=)
          (cant.setcci as 'equal rs imm rd))
         ((fx<)
          (emit-check-fixnum/imm as rs imm $ex.fx<)
          (cant.setcci as 'less rs imm rd))
         ((fx<=)
          (emit-check-fixnum/imm as rs imm $ex.fx<=)
          (cant.setcci as 'less-or-equal rs imm rd))
         ((fx>)
          (emit-check-fixnum/imm as rs imm $ex.fx>)
          (cant.setcci as 'greater rs imm rd))
         ((fx>=)
          (emit-check-fixnum/imm as rs imm $ex.fx>=)
          (cant.setcci as 'greater-or-equal rs imm rd))
         ((vector-ref:trusted vector-ref vector-like-ref bytevector-ref bytevector-like-ref typetag-set!)
          ;; TODO: it's possible to do better
          (cant.movi as imm $r.second) 
          (emit-op2 as name rs $r.second rd))
         (else
          (error 'op2imm "Unknown operation: " name)))))))

(define (emit-op2imm-branchf as name rs imm label)
  (if (label-value as label)
      (emit-timer-tick as))
  (case name
    ((eq?)
     (cond ((eq? imm #t)
            (cant.bcci as 'not-equal rs $imm.true label))
           ((eq? imm #f)
            (cant.bcci as 'not-equal rs $imm.false label))
           ((eq? imm '())
            (cant.bcci as 'not-equal rs $imm.null label))
           ((fence-imm? imm)
            (cant.bcci as 'not-equal rs (fence.native->fixnum imm) label))
           (else
            (error 'emit-op2imm-branchf "Unhandled immediate for eq?:" imm))))
    (else
     (let ((imm (fence.native->fixnum imm)))

       (define (fixnum-fast-case op trap-code)
         (let ((L1 (fence.make-label as))
               (L2 (fence.make-label as)))
           (cant.bcci   as 'low2-not-equal rs 0 L1)
           (cant.bcci   as op rs imm label)
           (cant.ba     as L2)
           (fence.label as L1)
           (if (not (= rs $r.result))
               (cant.mov as rs $r.result))
           (cant.movi   as imm $r.second)
           (cant.trap   as trap-code name)
           (cant.bcci   as 'equal $r.result $imm.false label)
           (fence.label as L2)))

       (case name
         ((<)
          (fixnum-fast-case 'greater-or-equal $m.numlt))
         ((<=)
          (fixnum-fast-case 'greater $m.numle))
         ((=)
          (fixnum-fast-case 'not-equal $m.numeq))
         ((>)
          (fixnum-fast-case 'less-or-equal $m.numgt))
         ((>=)
          (fixnum-fast-case 'less $m.numge))
         ((<:fix:fix)
          (cant.bcci as 'greater-or-equal rs imm label))
         ((<=:fix:fix)
          (cant.bcci as 'greater rs imm label))
         ((=:fix:fix)
          (cant.bcci as 'not-equal rs imm label))
         ((>:fix:fix)
          (cant.bcci as 'less-or-equal rs imm label))
         ((>=:fix:fix)
          (cant.bcci as 'less rs imm label))
         (else
          (error 'emit-op2imm-branchf "Unknown: " name)))))))
     
(define (emit-op2imm-check as name rs imm label)
  (if (runtime-safety-checking)
      (let ((imm (fence.native->fixnum imm)))
        (case name
          ((<:fix:fix)
           (cant.bcci as 'greater-or-equal rs imm label))
          ((<=:fix:fix)
           (cant.bcci as 'greater rs imm label))
          ((=:fix:fix)
           (cant.bcci as 'not-equal rs imm label))
          ((>:fix:fix)
           (cant.bcci as 'less-or-equal rs imm label))
          ((>=:fix:fix)
           (cant.bcci as 'less rs imm label))
          (else
           (error 'emit-op2imm-check "Unknown: " name))))))

(define (emit-op3 as name rs1 rs2 rs3)
  (assert (not (= rs1 $r.second)))
  (assert (not (= rs3 $r.second)))
  (assert (not (= rs2 $r.result)))
  (assert (not (= rs3 $r.result)))
  (case name
    ;; Vectors
    ((vector-set!:trusted)
     (emit-unchecked-structure-set as rs1 rs2 rs3 $tag.vector-tag #t))
    ((vector-set!:trusted:nwb)
     (emit-unchecked-structure-set as rs1 rs2 rs3 $tag.vector-tag #f))
    ((vector-set!)
     (emit-structure-set2 as rs1 rs2 rs3 $tag.vector-tag $hdr.vector $ex.vset))
    ((vector-like-set!)
     (emit-structure-set1 as rs1 rs2 rs3 $tag.vector-tag $ex.vlset))

    ;; Procedures
    ((procedure-set!)
     (emit-structure-set1 as rs1 rs2 rs3 $tag.procedure-tag $ex.pset))

    ;; Bytevectors
    ((bytevector-like-set!:trusted)
     (cant.shri as rs2 2 $r.tmp)         ; Byte index in tmp, rs2 is free
     (cant.add  as rs1 $r.tmp $r.tmp)    ; Byte pointer in tmp, not adjusted for tag, rs1 is free
     (cant.shri as rs3 2 $r.result)      ; Byte value in RESULT, rs3 is free (note RESULT may be rs1, rs2, or rs3 but that's OK)
     (cant.stbi as $r.result $r.tmp (- $bytewidth.wordsize $tag.bytevector-tag))
     (cant.movi as 0 $r.result))         ; clear untagged value
    ((bytevector-set!)
     (emit-check-tags       as rs1 $tag.bytevector-tag $hdr.bytevector $ex.bvset)
     (emit-check-fixnums    as rs2 rs3 $ex.bvset)
     (emit-check-byte-range as $tag.bytevector-tag rs1 rs2 $ex.bvset)
     (emit-op3              as 'bytevector-like-set!:trusted rs1 rs2 rs3))
    ((bytevector-like-set!)
     (emit-check-tag        as rs1 $tag.bytevector-tag $ex.bvlset)
     (emit-check-fixnums    as rs2 rs3 $ex.bvlset)
     (emit-check-byte-range as $tag.bytevector-tag rs1 rs2 $ex.bvlset)
     (emit-op3              as 'bytevector-like-set!:trusted rs1 rs2 rs3))

    ;; Strings
    ((ustring-set!:trusted)
     (case (nbuild-parameter 'target-string-rep)
       ((flat1)
        (cant.shri as rs2 2 $r.tmp)
        (cant.add  as rs1 $r.tmp $r.tmp)
        (cant.shri as rs3 8 $r.result)
        (cant.stbi as $r.result $r.tmp (- $bytewidth.wordsize $tag.bytevector-tag))
        (cant.movi as 0 $r.result))     ; clear untagged value
       ((flat4)
        (cant.add as rs1 rs2 $r.tmp)
        (if (= $bytewidth.wordsize 4)
            (cant.sti as rs3 $r.tmp (- $bytewidth.wordsize $tag.bytevector-tag))
            (error "ustring-set!:trusted not implemented for 64-bit systems"))) ; FIXME64
       (else
        (error "Unsupported string representation " (nbuild-parameter 'target-string-rep)))))

    (else
     (error "Unknown 3-argument primitive: " name))))

(define (emit-check-tag as reg tag exn)
  (if (runtime-safety-checking)
      (let ((L (fence.make-label as)))
        (cant.bcci      as 'low3-equal reg tag L)
        (emit-exception as exn reg)
        (fence.label    as L))))

; Using LDI and BCCI/low8-equal insulates against endianness
; dependency, but it may be a reasonable optimization to change that.

(define (emit-check-tags as reg ptr hdr exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as))
            (L2 (fence.make-label as)))
        (cant.bcci      as 'low3-equal reg ptr L1)
        (fence.label    as L2)
        (emit-exception as exn reg)
        (fence.label    as L1)
        (cant.ldi       as $r.result (- ptr) $r.tmp)
        (cant.bcci      as 'low8-equal $r.tmp hdr L2))))

(define (emit-check-fixnum as reg exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.bcci      as 'low2-equal reg 0 L1)
        (emit-exception as exn reg)
        (fence.label    as L1))))

(define (emit-check-positive-fixnum as reg exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as))
            (L2 (fence.make-label as)))
        (cant.bcci      as 'low2-equal reg 0 L1)
        (fence.label    as L2)
        (emit-exception as exn reg)
        (fence.label    as L1)
        (cant.bcci      as 'less reg 0 L2))))

(define (emit-check-fixnums as reg1 reg2 exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.or        as reg1 reg2 $r.tmp)
        (cant.bcci      as 'low2-equal $r.tmp 0 L1)
        (emit-exception as exn reg1 reg2)
        (fence.label    as L1))))

(define (emit-check-fixnum/imm as reg imm exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.bcci      as 'low2-equal reg 0 L1)
        (cant.movi      as imm $r.second)
        (emit-exception as exn reg $r.second)
        (fence.label    as L1))))

; r_obj must be an object with the given tag.
; r_idx must be a fixnum.

(define (emit-check-word-range as tag r_obj r_idx exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.ldi       as r_obj (- tag) $r.tmp)
        (cant.shrai     as $r.tmp 8 $r.tmp)
        (cant.bcc       as 'unsigned-less r_idx $r.tmp L1)
        (emit-exception as exn r_obj r_idx)
        (fence.label    as L1))))

(define (emit-check-byte-range as tag r_obj r_idx exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.ldi       as r_obj (- tag) $r.tmp)
        (cant.shrai     as $r.tmp 6 $r.tmp)
        (cant.andni     as $r.tmp 3 $r.tmp)
        (cant.bcc       as 'unsigned-less r_idx $r.tmp L1)
        (emit-exception as exn r_obj r_idx)
        (fence.label    as L1))))

(define (emit-check-character as r exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as)))
        (cant.bcci      as 'low8-equal r $imm.character L1)
        (emit-exception as exn r)
        (fence.label    as L1))))

(define (emit-check-characters as r1 r2 exn)
  (if (runtime-safety-checking)
      (let ((L1 (fence.make-label as))
            (L2 (fence.make-label as)))
        (cant.bcci      as 'low8-equal r1 $imm.character L1)
        (fence.label    as L2)
        (emit-exception as exn r1 r2)
        (fence.label    as L1)
        (cant.bcci      as 'low8-not-equal r2 $imm.character L2))))

(define (emit-test-tags as reg rd ptr hdr)
  (let ((L1 (fence.make-label as))
        (L2 (fence.make-label as)))
    (cant.bcci   as 'low3-equal reg ptr L1)
    (cant.movi   as $imm.false rd)
    (cant.ba     as L2)
    (fence.label as L1)
    (cant.ldi    as reg (- ptr) $r.tmp)
    (cant.setcci as 'low8-equal $r.tmp hdr rd)
    (fence.label as L2)))

(define (emit-test-tags-branchf as reg label ptr hdr)
  (cant.bcci   as 'low3-not-equal reg ptr label)
  (cant.ldi    as reg (- ptr) $r.tmp)
  (cant.bcci   as 'low8-not-equal $r.tmp hdr label))

(define (emit-structure-length1 as rs rd tag exn)
  (emit-check-tag as rs tag exn)
  (cant.ldi       as rs (- tag) rd)
  (cant.shri      as rd 8 rd))

(define (emit-structure-length2 as rs rd tag hdr exn)
  (emit-check-tags as rs tag hdr exn)
  (cant.ldi        as rs (- tag) rd)
  (cant.shri       as rd 8 rd))

(define (emit-structure-ref1 as rs1 rs2 rd tag exn)
  (emit-check-tag        as rs1 tag exn)
  (emit-check-fixnum     as rs2 exn)
  (emit-check-word-range as tag rs1 rs2 exn)
  (emit-unchecked-structure-ref as rs1 rs2 rd tag))

(define (emit-structure-ref2 rs1 rs2 rd tag hdr exn)
  (emit-check-tags       as rs1 tag hdr exn)
  (emit-check-fixnum     as rs2 exn)
  (emit-check-word-range as tag rs1 rs2 exn)
  (emit-unchecked-structure-ref as rs1 rs2 rd tag))

(define (emit-structure-set1 as rs1 rs2 rs3 tag exn)
  (emit-check-tag        as rs1 tag exn)
  (emit-check-fixnum     as rs2 exn)
  (emit-check-word-range as tag rs1 rs2 exn)
  (emit-unchecked-structure-set as rs1 rs2 rs3 tag #t))

(define (emit-structure-set2 as rs1 rs2 rs3 tag hdr exn)
  (emit-check-tags       as rs1 tag hdr exn)
  (emit-check-fixnum     as rs2 exn)
  (emit-check-word-range as tag rs1 rs2 exn)
  (emit-unchecked-structure-set as rs1 rs2 rs3 tag #t))

(define (emit-unchecked-structure-ref as rs1 rs2 rd tag)
  (cant.add as rs1 rs2 rd)
  (cant.ldi as rd (- $bytewidth.wordsize tag) rd))

(define (emit-unchecked-structure-set as rs1 rs2 rs3 tag barrier?)
  (cant.add           as rs1 rs2 $r.tmp)
  (cant.sti           as rs3 $r.tmp (- $bytewidth.wordsize tag))
  (if barrier?
      (emit-write-barrier as rs1 rs3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Cant definition.
;
; A back-end must override all the functions defined below.
;
; Register naming in this interface:
;  - MAL registers $r.reg0..$r.regn are here numbered 0..n
;  - $r.result, $r.second, $r.globals, $r.stkp, and $r.tmp have negative values
;
; This interface makes no assumptions about whether a MAL register is mapped
; to a CPU register or a slot in the globals array.

; Used only within this file for the stubs.

(define $cant$ (make-bytevector 4 0))

; Emit a label.  Normally this just calls emit-label!.

(define (cant.label as L)
  (if (fence.label? L)
      (list-asm-label L))
  (emit-label! as L))

; End of the code vector.  Normally this does nothing.

(define (cant.end as)
  #t)

; Invoke the millicode procedure denoted by "code" (a fixnum).
; Arguments must already be in millicode argument registers.
; "name" is the name corresponding to the code, used only for
; documentation.
;
; Optionally a numeric value representing an exception code
; can be passed here; if the entrypoint is not the general
; exception handler then no value should be provided.

(define (cant.trap as code name . rest) 
  (list-cant "trap" `(#f ,code ,name))
  (emit! as $cant$))

; Insert a no-op that will bring the code stream closer to alignment.
; This could be a one-byte instruction, or a two-byte instruction on
; eg Thumb/Thumb2 where the stream alignment is always two or four
; bytes.

(define (cant.nop as)
  (list-cant "nop" '(#f))
  (emit! as $cant$))

; Load a value from memory into an integer register: rd=*(base+offset).

(define (cant.ldi as base offset rd)
  (list-cant "ldi" `(#f ,(list-regname base) ,offset ,(list-regname rd)))
  (emit! as $cant$))

; Load a byte value from memory and zero-extend into an integer register:
; rd=zeroExtend(*(byte*)(base+offset)).

(define (cant.ldbiz as base offset rd)
  (list-cant "ldb" `(#f ,(list-regname base) ,offset ,(list-regname rd)))
  (emit! as $cant$))

; Load a floating register from memory.  The effective address must be
; 8-byte aligned.

(define (cant.fldi as base offset frd)
  (list-cant "fldi" `(#f ,(list-regname base) ,offset ,(list-fregname frd)))
  (emit! as $cant$))

; Store a value from an integer register into memory without 
; a write barrier: *(base+offset)=rs.

(define (cant.sti as rs base offset)
  (list-cant "sti" `(#f ,(list-regname rs) ,(list-regname base) ,offset))
  (emit! as $cant$))

; Store a byte value from an integer register into memory without a
; write barrier.  The low byte of the source register is used:
; *(byte*)(base+offset)=(byte)rs.

(define (cant.stbi as rs base offset)
  (list-cant "sti" `(#f ,(list-regname rs) ,(list-regname base) ,offset))
  (emit! as $cant$))

; Store a floating register to memory.  The effective address must be
; 8-byte aligned

(define (cant.fsti as frs base offset)
  (list-cant "fsti" `(#f ,(list-fregname frs) ,(list-regname base) ,offset))
  (emit! as $cant$))

; Move an immediate to an integer register.
; imm may be larger than typical "immediate" fields on RISC machines.

(define (cant.movi as imm rd)
  (list-cant "movi" `(#f ,imm ,(list-regname rd)))
  (emit! as $cant$))

; Move an integer register to another.

(define (cant.mov as rs rd)
  (list-cant "mov" `(#f ,(list-regname rs) ,(list-regname rd)))
  (emit! as $cant$))

; Jump to the return address in the stack at byte offset "offset",
; generated by cant.setrtn/strtn, cant.setrtn/strtn/jump, or
; cant.setrtn/strtn/branch.

(define (cant.ldrtn/return as offset)
  (list-cant "ldrtn/return" `(#f ,offset))
  (emit! as $cant$))

; Generate a return address for the label and store it into the current frame at
; byte offset "offset".  Only cant.ldrtn/return will make use of that return address.

(define (cant.setrtn/strtn as L offset)
  (list-cant "setrtn/strtn" `(#f ,(list-labelname L) ,offset))
  (emit! as $cant$))

; Branch always to L, which must be within the code vector of the
; branching instruction.

(define (cant.ba as L)
  (list-cant "ba" `(#f ,(list-labelname L)))
  (emit! as $cant$))

; Jump always to L, which must be within the code vector of REG0.
; L may be #f, in which we jump to the beginning of that code vector.
; Destroys $r.tmp.

(define (cant.jump as L)
  (list-cant "jump" `(#f ,(if L (list-labelname L) #f)))
  (emit! as $cant$))

; If r and imm compare according to condition then branch to L, which
; must be within the code vector of the branching instruction.
;
; imm is an integer value, but it is not guaranteed to fit into the
; immediate field of a RISC instruction.
;
; Condition is a symbol, one of:
;
;  - equal
;  - not-equal
;  - less
;  - greater
;  - less-or-equal
;  - greater-or-equal
;  - unsigned-less
;  - low1-equal
;  - low1-not-equal
;  - low2-equal
;  - low2-not-equal
;  - low3-equal
;  - low3-not-equal
;  - low8-equal
;  - low8-not-equal
;
; For less, greater, less-or-equal, and greater-or-equal the
; comparison is always signed.
;
; For low<n>-{not-}equal, imm is compared to the low n bits of 
; the value in r, and imm must fit in n bits.  For the others 
; the entire value of r is used in the comparison.

(define (cant.bcci as condition r imm L)
  (list-cant "bcci" `(#f ,condition ,(list-regname r) ,imm ,(list-labelname L)))
  (emit! as $cant$))

; As bcci, but with the second operand in a register.

(define (cant.bcc as condition r1 r2 L)
  (list-cant "bcc" `(#f ,condition ,(list-regname r1) ,(list-regname r2) ,(list-labelname L)))
  (emit! as $cant$))

; If r1 and r2 compare true according to "condition" then set rd to #t,
; otherwise set rd to #f.  The conditions are as for bcci.

(define (cant.setcc as condition r1 r2 rd)
  (list-cant "setcc" `(#f ,condition ,(list-regname r1) ,(list-regname r2) ,(list-regname rd)))
  (emit! as $cant$))

; If r1 and imm compare true according to "condition" then set rd to #t,
; otherwise set rd to #f.  The conditions are as for bcci.

(define (cant.setcci as condition r imm rd)
  (list-cant "setcci" `(#f ,condition ,(list-regname r) ,imm ,(list-regname rd)))
  (emit! as $cant$))

; Add signed immediate.  The immediate can be any size up to 32 bits.

(define (cant.addi as rs imm rd)
  (list-cant "addi" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

; Add signed immediate and branch to L on no overflow; do not update
; rd on overflow.  The immediate can be any size up to 32 bits.

(define (cant.addi/bnv as rs imm rd L)
  (list-cant "addi/bnv" `(#f ,(list-regname rs) ,imm ,(list-regname rd) ,(list-labelname L)))
  (emit! as $cant$))

; Subtract signed immediate.  The immediate can be any size up to 32 bits.

(define (cant.subi as rs imm rd)
  (list-cant "subi" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

; Subtract signed immediate and branch to L on no overflow; do not update
; rd on overflow.  The immediate can be any size up to 32 bits.

(define (cant.subi/bnv as rs imm rd L)
  (list-cant "subi/bnv" `(#f ,(list-regname rs) ,imm ,(list-regname rd) ,(list-labelname L)))
  (emit! as $cant$))

; Shift left and right immediate.  The immediate should be restricted
; to the number of bits in a word.

(define (cant.shli as rs imm rd)
  (list-cant "shli" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

(define (cant.shri as rs imm rd)
  (list-cant "shri" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

(define (cant.shrai as rs imm rd)
  (list-cant "shrai" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

; And with complemented immediate.

(define (cant.andni as rs imm rd)
  (list-cant "andni" `(#f ,(list-regname rs) ,imm ,(list-regname rd)))
  (emit! as $cant$))

; Add registers.

(define (cant.add as rs1 rs2 rd)
  (list-cant "add" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Add and branch to L on no overflow; do not update rd on overflow.

(define (cant.add/bnv as rs1 rs2 rd L)
  (list-cant "add/bnv" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd) ,(list-labelname L)))
  (emit! as $cant$))

; Subtract registers.

(define (cant.sub as rs1 rs2 rd)
  (list-cant "sub" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Subtract and branch to L on no overflow; do not update rd on overflow.

(define (cant.sub/bnv as rs1 rs2 rd L)
  (list-cant "sub/bnv" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd) ,(list-labelname L)))
  (emit! as $cant$))

; Multiply registers 

(define (cant.mul as rs1 rs2 rd)
  (list-cant "mul" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Multiply and branch to L on no overflow; do not update rd on overflow.

(define (cant.mul/bnv as rs1 rs2 rd L)
  (list-cant "mul/bnv" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd) ,(list-labelname L)))
  (emit! as $cant$))

; Bitwise and.

(define (cant.and as rs1 rs2 rd)
  (list-cant "and" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise inclusive or.

(define (cant.or as rs1 rs2 rd)
  (list-cant "or" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise exclusive or.

(define (cant.xor as rs1 rs2 rd)
  (list-cant "xor" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise left shift.

(define (cant.shl as rs1 rs2 rd)
  (list-cant "shl" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise right shift, insert zeroes on the left.

(define (cant.shr as rs1 rs2 rd)
  (list-cant "shr" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise right shift, insert copies of the sign bit on the left.

(define (cant.shra as rs1 rs2 rd)
  (list-cant "shra" `(#f ,(list-regname rs1) ,(list-regname rs2) ,(list-regname rd)))
  (emit! as $cant$))

; Bitwise one's complement.

(define (cant.not as rs rd)
  (list-cant "not" `(#f ,(list-regname rs) ,(list-regname rd)))
  (emit! as $cant$))

; Floating register add

(define (cant.fadd as frs1 frs2 frd)
  (list-cant "fadd" `(#f ,(list-fregname frs1) ,(list-fregname frs2) ,(list-fregname frd)))
  (emit! as $cant$))

; Floating register subtract 

(define (cant.fsub as frs1 frs2 frd)
  (list-cant "fsub" `(#f ,(list-fregname frs1) ,(list-fregname frs2) ,(list-fregname frd)))
  (emit! as $cant$))

; Floating register multiply

(define (cant.fmul as frs1 frs2 frd)
  (list-cant "fmul" `(#f ,(list-fregname frs1) ,(list-fregname frs2) ,(list-fregname frd)))
  (emit! as $cant$))

; Floating register divide

(define (cant.fdiv as frs1 frs2 frd)
  (list-cant "fdiv" `(#f ,(list-fregname frs1) ,(list-fregname frs2) ,(list-fregname frd)))
  (emit! as $cant$))

; Floating compare.
;
; Conditions are:
;   equal
;   not-equal
;   less
;   less-or-equal
;   greater
;   greater-or-equal

(define (cant.fsetcc as op frs1 frs2 rd)
  (list-cant "fsetcc" `(#f ,op ,(list-fregname frs1) ,(list-fregname frs2) ,(list-regname rd)))
  (emit! as $cant$))

;;; Instructions that support desirable peephole optimizations.

; Combines these instructions, L can be #f:
;   setrtn/strtn M
;   jump         L
;   .align       a
;   .label       M

(define (cant.setrtn/strtn/jump as L alignment offset)
  (list-cant "setrtn/strtn/jump" `(#f ,L ,alignment ,offset))
  (emit! as $cant$))

; Combines these instructions with r some temp:
;   setrtn/strtn M
;   branch       L
;   .align       a
;   .label       M

(define(cant.setrtn/strtn/branch as L alignment offset)
  (list-cant "setrtn/strtn/branch" `(#f ,L ,alignment ,offset))
  (emit! as $cant$))

; eof
