; Copyright 1998 William Clinger.
;
; $Id$
;
; Local optimizations for MacScheme machine assembly code.
;
; Suppress nop instructions.
; Suppress save, restore, and pop instructions whose operand is -1.
; Suppress redundant stores.
; Suppress definitions (primarily loads) of dead registers.
;
; To perform these optimizations, the basic block must be traversed
; both forwards and backwards.
; The forward traversal keeps track of registers that were defined
; by a load.
; The backward traversal keeps track of live registers.

(define filter-basic-blocks
  
  (let* ((*nregs* *nregs*) ; locals might be faster than globals
         
         ; largest mnemonic + 1
         
         (dispatch-table-size *number-of-mnemonics*)
         
         ; During the forwards traversal:
         ;    (vector-ref registers i) = #f
         ;        means the content of register i is unknown
         ;    (vector-ref registers i) = j
         ;        means register was defined by load i,j
         ;
         ; During the backwards traversal:
         ;    (vector-ref registers i) = #f means register i is dead
         ;    (vector-ref registers i) = #t means register i is live
         
         (registers (make-vector *nregs* #f))
         
         ; Dispatch table for the forwards traversal.
         
         (forward-table (make-vector dispatch-table-size))
         
         ; Dispatch table for the backwards traversal.
         
         (backward-table (make-vector dispatch-table-size)))
    
    (define (vector-fill! v x)
      (subvector-fill! v 0 (vector-length v) x))
    
    (define (subvector-fill! v i j x)
      (if (< i j)
          (begin (vector-set! v i x)
                 (subvector-fill! v (+ i 1) j x))))
    
    (define (kill-stack! j)
      (do ((i 0 (+ i 1)))
          ((= i *nregs*))
          (let ((x (vector-ref registers i)))
            (if (and x (= x j))
                (vector-set! registers i #f)))))
    
    ; Dispatch procedure for the forwards traversal.
    
    (define (forwards instructions filtered)
      (if (null? instructions)
          (begin (vector-fill! registers #f)
                 (vector-set! registers 0 #t)
                 (backwards0 filtered '()))
          (let ((instruction (car instructions)))
            ((vector-ref forward-table (instruction.op instruction))
             (cdr instructions)
             filtered
             instruction))))
    
    ; Dispatch procedure for the backwards traversal.
    
    (define (backwards instructions filtered)
      (if (null? instructions)
          filtered
          (let ((instruction (car instructions)))
            ((vector-ref backward-table (instruction.op instruction))
             (cdr instructions)
             filtered
             instruction))))
    
    ; Given a list of instructions in reverse order, whose first
    ; element is the last instruction of a basic block,
    ; and a filtered list of instructions in forward order,
    ; returns a filtered list of instructions in the correct order.
    
    (define (backwards0 instructions filtered)
      (if (null? instructions)
          filtered
          (let* ((instruction (car instructions))
                 (mnemonic (instruction.op instruction)))
            (cond ((or (eqv? instruction $.label)
                       (eqv? instruction $.proc)
                       (eqv? instruction $.cont)
                       (eqv? instruction $.align))
                   (backwards0 (cdr instructions)
                               (cons instruction filtered)))
                  ; all registers are dead at a $return
                  ((eqv? mnemonic $return)
                   (vector-fill! registers #f)
                   (vector-set! registers 0 #t)
                   (backwards (cdr instructions)
                              (cons instruction filtered)))
                  ; all but the argument registers are dead at an $invoke
                  ((eqv? mnemonic $invoke)
                   (let ((n+1 (min *nregs*
                                   (+ (instruction.arg1 instruction) 1))))
                     (subvector-fill! registers 0 n+1 #t)
                     (subvector-fill! registers n+1 *nregs* #f)
                     (backwards (cdr instructions)
                                (cons instruction filtered))))
                  ; the compiler says which registers are live at the
                  ; target of $skip, $branch, $branchf, or $jump
                  ((or (eqv? mnemonic $skip)
                       (eqv? mnemonic $branch))
                   (let ((n+1 (min *nregs*
                                   (+ (instruction.arg2 instruction) 1))))
                     (subvector-fill! registers 0 n+1 #t)
                     (subvector-fill! registers n+1 *nregs* #f)
                     (backwards (cdr instructions)
                                (cons instruction filtered))))
                  ((eqv? mnemonic $jump)
                   (let ((n+1 (min *nregs*
                                   (+ (instruction.arg3 instruction) 1))))
                     (subvector-fill! registers 0 n+1 #t)
                     (subvector-fill! registers n+1 *nregs* #f)
                     (backwards (cdr instructions)
                                (cons instruction filtered))))
                  ; the live registers at the target of a $branchf must be
                  ; combined with the live registers at the $branchf
                  ((eqv? mnemonic $branchf)
                   (let ((n+1 (min *nregs*
                                   (+ (instruction.arg2 instruction) 1))))
                     (subvector-fill! registers 0 n+1 #t)
                     (backwards (cdr instructions)
                                (cons instruction filtered))))
                  (else (backwards instructions filtered))))))
    
    ; Dispatch procedures for the forwards traversal.
    
    (vector-fill! forward-table
                  (lambda (instructions filtered instruction)
                    (forwards instructions (cons instruction filtered))))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-fill! registers #f)
                  (forwards instructions (cons instruction filtered)))))
      (for-each (lambda (i) (vector-set! forward-table i proc))
                
                ; Instructions that end a basic block.
                
                (list $invoke
                      $return
                      $skip
                      $branch
                      $branchf
                      $jump
                      $.align
                      $.proc
                      $.cont
                      $.label)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (forwards instructions filtered))))
      
      ; No operation.
      
      (vector-set! forward-table $nop proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (if (negative? (instruction.arg1 instruction))
                      (forwards instructions filtered)
                      (begin
                       (vector-fill! registers #f)
                       (forwards instructions (cons instruction filtered)))))))
      (for-each (lambda (i) (vector-set! forward-table i proc))
                
                ; Stack frame instructions, suppressed if operand is -1.
                ; Also instructions viewed as killing all registers.
                
                (list $save $restore $pop $args>=)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (let ((i (instruction.arg1 instruction))
                        (j (instruction.arg2 instruction)))
                    (if (eqv? (vector-ref registers i) j)
                        ; suppress redundant store
                        (forwards instructions filtered)
                        ; registers that held stack slot j will no longer
                        ; hold stack slot j after the store
                        (begin
                         (kill-stack! j)
                         (vector-set! registers i j)
                         (forwards instructions
                                   (cons instruction filtered))))))))
      (vector-set! forward-table
                   $store
                   proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (let ((i (instruction.arg1 instruction))
                        (j (instruction.arg2 instruction)))
                    (if (eqv? (vector-ref registers i) j)
                        ; suppress redundant load
                        ; does the compiler ever do this?
                        (forwards instructions filtered)
                        (begin
                         (vector-set! registers i j)
                         (forwards instructions
                                   (cons instruction filtered))))))))
      (vector-set! forward-table
                   $load
                   proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (kill-stack! (instruction.arg1 instruction))
                  (forwards instructions (cons instruction filtered)))))
      (vector-set! forward-table
                   $setstk
                   proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-set! registers (instruction.arg1 instruction) #f)
                  (forwards instructions (cons instruction filtered)))))
      (vector-set! forward-table
                   $setreg
                   proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-set! registers (instruction.arg2 instruction) #f)
                  (forwards instructions (cons instruction filtered)))))
      (vector-set! forward-table
                   $movereg
                   proc))
    
    ; Dispatch procedures for the backwards traversal.
    
    (vector-fill! backward-table
                  (lambda (instructions filtered instruction)
                    (backwards instructions (cons instruction filtered))))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (backwards0 (cons instruction instructions) filtered))))
      (for-each (lambda (i) (vector-set! backward-table i proc))
                
                ; Instructions that end a basic block.
                
                (list $invoke
                      $return
                      $skip
                      $branch
                      $branchf)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (backwards0 instructions (cons instruction filtered)))))
      (for-each (lambda (i) (vector-set! backward-table i proc))
                
                ; Labels that begin a basic block.
                
                (list $jump
                      $.align
                      $.proc
                      $.cont
                      $.label)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-set! registers (instruction.arg2 instruction) #t)
                  (backwards instructions (cons instruction filtered)))))
      (vector-set! backward-table $op2 proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-set! registers (instruction.arg2 instruction) #t)
                  (vector-set! registers (instruction.arg3 instruction) #t)
                  (backwards instructions (cons instruction filtered)))))
      (vector-set! backward-table $op3 proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-set! registers (instruction.arg1 instruction) #t)
                  (backwards instructions (cons instruction filtered)))))
      (for-each (lambda (i) (vector-set! backward-table i proc))
                
                ; Instructions that use their first operand.
                
                (list $store $reg)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (let ((i (instruction.arg1 instruction)))
                    (if (not (vector-ref registers i))
                        (backwards instructions filtered)
                        (begin
                         (vector-set! registers i #f)
                         (backwards instructions
                                    (cons instruction filtered))))))))
      (for-each (lambda (i) (vector-set! backward-table i proc))
                
                ; Instructions that kill their first operand.
                
                (list $load $setreg)))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (let ((i (instruction.arg1 instruction))
                        (j (instruction.arg2 instruction)))
                    (if (or (= i j)
                            (not (vector-ref registers j)))
                        (backwards instructions filtered)
                        (begin
                         (vector-set! registers i #t)
                         (vector-set! registers j #f)
                         (backwards instructions
                                    (cons instruction filtered))))))))
      (vector-set! backward-table $movereg proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (subvector-fill! registers
                                   0
                                   (min *nregs*
                                        (+ 1 (instruction.arg2 instruction)))
                                   #t)
                  (backwards instructions (cons instruction filtered)))))
      (vector-set! backward-table $lambda proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (subvector-fill! registers
                                   0
                                   (min *nregs*
                                        (+ 1 (instruction.arg1 instruction)))
                                   #t)
                  (backwards instructions (cons instruction filtered)))))
      (vector-set! backward-table $lexes proc))
    
    (let ((proc (lambda (instructions filtered instruction)
                  (vector-fill! registers #t)
                  (backwards instructions (cons instruction filtered)))))
      (vector-set! backward-table $args>= proc))
    
    (lambda (instructions)
      (vector-fill! registers #f)
      (forwards instructions '()))))
