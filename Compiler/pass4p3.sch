; Copyright 1998 William Clinger.
;
; $Id$
;
; 5 June 1999.
;
; Local optimizations for MacScheme machine assembly code.
;
; Branch tensioning.
; Suppress nop instructions.
; Suppress save, restore, and pop instructions whose operand is -1.
; Suppress redundant stores.
; Suppress definitions (primarily loads) of dead registers.
;
; Note:  Twobit never generates a locally redundant load or store,
; so this code must be tested with a different code generator.
;
; To perform these optimizations, the basic block must be traversed
; both forwards and backwards.
; The forward traversal keeps track of registers that were defined
; by a load.
; The backward traversal keeps track of live registers.

(define filter-basic-blocks
  
  (let* ((suppression-message
          "Local optimization detected a useless instruction.")
         
         ; Each instruction is mapping to an encoding of the actions
         ; to be performed when it is encountered during the forward
         ; or backward traversal.
         
         (forward:normal                   0)
         (forward:nop                      1)
         (forward:ends-block               2)
         (forward:interesting              3)
         (forward:kills-all-registers      4)
         (forward:nop-if-arg1-is-negative  5)
         
         (backward:normal                  0)
         (backward:ends-block              1)
         (backward:begins-block            2)
         (backward:uses-arg1               4)
         (backward:uses-arg2               8)
         (backward:uses-arg3              16)
         (backward:kills-arg1             32)
         (backward:kills-arg2             64)
         (backward:uses-many             128)
         
         ; largest mnemonic + 1
         
         (dispatch-table-size *number-of-mnemonics*)
         
         ; Dispatch table for the forwards traversal.
         
         (forward-table (make-bytevector dispatch-table-size))
         
         ; Dispatch table for the backwards traversal.
         
         (backward-table (make-bytevector dispatch-table-size)))
    
    (do ((i 0 (+ i 1)))
        ((= i dispatch-table-size))
        (bytevector-set! forward-table i forward:normal)
        (bytevector-set! backward-table i backward:normal))
    
    (bytevector-set! forward-table $nop     forward:nop)
    
    (bytevector-set! forward-table $invoke  forward:ends-block)
    (bytevector-set! forward-table $return  forward:ends-block)
    (bytevector-set! forward-table $skip    forward:ends-block)
    (bytevector-set! forward-table $branch  forward:ends-block)
    (bytevector-set! forward-table $branchf forward:ends-block)
    (bytevector-set! forward-table $jump    forward:ends-block)
    (bytevector-set! forward-table $.align  forward:ends-block)
    (bytevector-set! forward-table $.proc   forward:ends-block)
    (bytevector-set! forward-table $.cont   forward:ends-block)
    (bytevector-set! forward-table $.label  forward:ends-block)
    
    (bytevector-set! forward-table $store   forward:interesting)
    (bytevector-set! forward-table $load    forward:interesting)
    (bytevector-set! forward-table $setstk  forward:interesting)
    (bytevector-set! forward-table $setreg  forward:interesting)
    (bytevector-set! forward-table $movereg forward:interesting)
    (bytevector-set! forward-table $const/setreg
                                            forward:interesting)
    
    (bytevector-set! forward-table $args>=  forward:kills-all-registers)
    (bytevector-set! forward-table $popstk  forward:kills-all-registers)
    
    ; These instructions also kill all registers.
    
    (bytevector-set! forward-table $save    forward:nop-if-arg1-is-negative)
    (bytevector-set! forward-table $restore forward:nop-if-arg1-is-negative)
    (bytevector-set! forward-table $pop     forward:nop-if-arg1-is-negative)
  
    (bytevector-set! backward-table $invoke  backward:ends-block)
    (bytevector-set! backward-table $return  backward:ends-block)
    (bytevector-set! backward-table $skip    backward:ends-block)
    (bytevector-set! backward-table $branch  backward:ends-block)
    (bytevector-set! backward-table $branchf backward:ends-block)
    
    (bytevector-set! backward-table $jump    backward:begins-block) ; [sic]
    (bytevector-set! backward-table $.align  backward:begins-block)
    (bytevector-set! backward-table $.proc   backward:begins-block)
    (bytevector-set! backward-table $.cont   backward:begins-block)
    (bytevector-set! backward-table $.label  backward:begins-block)
    
    (bytevector-set! backward-table $op2     backward:uses-arg2)
    (bytevector-set! backward-table $op3     (fxlogior backward:uses-arg2
                                                     backward:uses-arg3))
    (bytevector-set! backward-table $check   (fxlogior
                                              backward:uses-arg1
                                              (fxlogior backward:uses-arg2
                                                      backward:uses-arg3)))
    (bytevector-set! backward-table $trap    (fxlogior
                                              backward:uses-arg1
                                              (fxlogior backward:uses-arg2
                                                      backward:uses-arg3)))
    (bytevector-set! backward-table $store   backward:uses-arg1)
    (bytevector-set! backward-table $reg     backward:uses-arg1)
    (bytevector-set! backward-table $load    backward:kills-arg1)
    (bytevector-set! backward-table $setreg  backward:kills-arg1)
    (bytevector-set! backward-table $movereg (fxlogior backward:uses-arg1
                                                     backward:kills-arg2))
    (bytevector-set! backward-table $const/setreg
                                             backward:kills-arg2)
    (bytevector-set! backward-table $lambda  backward:uses-many)
    (bytevector-set! backward-table $lexes   backward:uses-many)
    (bytevector-set! backward-table $args>=  backward:uses-many)
    
    (lambda (instructions)
      
      (let* ((*nregs* *nregs*) ; locals might be faster than globals
             
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
             
             ; During the forwards traversal, the label of a block that
             ; falls through into another block or consists of a skip
             ; to another block is mapped to another label.
             ; This mapping is implemented by a hash table.
             ; Before the backwards traversal, the transitive closure
             ; is computed.  The graph has no cycles, and the maximum
             ; out-degree is 1, so this is easy.
             
             (label-table (make-hashtable (lambda (n) n) assv)))
        
        (define (compute-transitive-closure!)
          (define (lookup x)
            (let ((y (hashtable-get label-table x)))
              (if y
                  (lookup y)
                  x)))
          (hashtable-for-each (lambda (x y)
                                (hashtable-put! label-table x (lookup y)))
                              label-table))
        
        ; Don't use this procedure until the preceding procedure
        ; has been called.
        
        (define (lookup-label x)
          (hashtable-fetch label-table x x))
        
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
                     (compute-transitive-closure!)
                     (backwards0 filtered '()))
              (let* ((instruction (car instructions))
                     (instructions (cdr instructions))
                     (op (instruction.op instruction))
                     (flags (bytevector-ref forward-table op)))
                (cond ((eqv? flags forward:normal)
                       (forwards instructions (cons instruction filtered)))
                      ((eqv? flags forward:nop)
                       (forwards instructions filtered))
                      ((eqv? flags forward:nop-if-arg1-is-negative)
                       (if (negative? (instruction.arg1 instruction))
                           (forwards instructions filtered)
                           (begin (vector-fill! registers #f)
                                  (forwards instructions
                                            (cons instruction filtered)))))
                      ((eqv? flags forward:kills-all-registers)
                       (vector-fill! registers #f)
                       (forwards instructions
                                 (cons instruction filtered)))
                      ((eqv? flags forward:ends-block)
                       (vector-fill! registers #f)
                       (if (eqv? op $.label)
                           (forwards-label instruction
                                           instructions
                                           filtered)
                           (forwards instructions
                                     (cons instruction filtered))))
                      ((eqv? flags forward:interesting)
                       (cond ((eqv? op $setreg)
                              (vector-set! registers
                                           (instruction.arg1 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $const/setreg)
                              (vector-set! registers
                                           (instruction.arg2 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $movereg)
                              (vector-set! registers
                                           (instruction.arg2 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $setstk)
                              (kill-stack! (instruction.arg1 instruction))
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $load)
                              (let ((i (instruction.arg1 instruction))
                                    (j (instruction.arg2 instruction)))
                                (if (eqv? (vector-ref registers i) j)
                                    ; Suppress redundant load.
                                    ; Should never happen with Twobit.
                                    (suppress-forwards instruction
                                                       instructions
                                                       filtered)
                                    (begin (vector-set! registers i j)
                                           (forwards instructions
                                                     (cons instruction
                                                           filtered))))))
                             ((eqv? op $store)
                              (let ((i (instruction.arg1 instruction))
                                    (j (instruction.arg2 instruction)))
                                (if (eqv? (vector-ref registers i) j)
                                    ; Suppress redundant store.
                                    ; Should never happen with Twobit.
                                    (suppress-forwards instruction
                                                       instructions
                                                       filtered)
                                    (begin (kill-stack! j)
                                           (forwards instructions
                                                     (cons instruction
                                                           filtered))))))
                             (else
                              (local-optimization-error op))))
                      (else
                       (local-optimization-error op))))))
        
        ; Enters labels into a table for branch tensioning.
        
        (define (forwards-label instruction1 instructions filtered)
          (let ((label1 (instruction.arg1 instruction1)))
            (if (null? instructions)
                ; This is ok provided the label is unreachable.
                (forwards instructions (cdr filtered))
                (let loop ((instructions instructions)
                           (filtered (cons instruction1 filtered)))
                  (let* ((instruction (car instructions))
                         (op (instruction.op instruction))
                         (flags (bytevector-ref forward-table op)))
                    (cond ((eqv? flags forward:nop)
                           (loop (cdr instructions) filtered))
                          ((and (eqv? flags forward:nop-if-arg1-is-negative)
                                (negative? (instruction.arg1 instruction)))
                           (loop (cdr instructions) filtered))
                          ((eqv? op $.label)
                           (let ((label2 (instruction.arg1 instruction)))
                             (hashtable-put! label-table label1 label2)
                             (forwards-label instruction
                                             (cdr instructions)
                                             (cdr filtered))))
                          ((eqv? op $skip)
                           (let ((label2 (instruction.arg1 instruction)))
                             (hashtable-put! label-table label1 label2)
                             ; We can't get rid of the skip instruction
                             ; because control might fall into this block,
                             ; but we can get rid of the label.
                             (forwards instructions (cdr filtered))))
                          (else
                           (forwards instructions filtered))))))))
        
        ; Dispatch procedure for the backwards traversal.
        
        (define (backwards instructions filtered)
          (if (null? instructions)
              filtered
              (let* ((instruction (car instructions))
                     (instructions (cdr instructions))
                     (op (instruction.op instruction))
                     (flags (bytevector-ref backward-table op)))
                (cond ((eqv? flags backward:normal)
                       (backwards instructions (cons instruction filtered)))
                      ((eqv? flags backward:ends-block)
                       (backwards0 (cons instruction instructions)
                                   filtered))
                      ((eqv? flags backward:begins-block)
                       (backwards0 instructions
                                   (cons instruction filtered)))
                      ((eqv? flags backward:uses-many)
                       (cond ((or (eqv? op $lambda)
                                  (eqv? op $lexes))
                              (let ((live
                                     (if (eqv? op $lexes)
                                         (instruction.arg1 instruction)
                                         (instruction.arg2 instruction))))
                                (subvector-fill! registers
                                                 0
                                                 (min *nregs* (+ 1 live))
                                                 #t)
                                (backwards instructions
                                           (cons instruction filtered))))
                             ((eqv? op $args>=)
                              (vector-fill! registers #t)
                              (backwards instructions
                                         (cons instruction filtered)))
                             (else
                              (local-optimization-error op))))
                      ((and (eqv? (fxlogand flags backward:kills-arg1)
                                  backward:kills-arg1)
                            (not (vector-ref registers
                                             (instruction.arg1 instruction))))
                       ; Suppress initialization of dead register.
                       (suppress-backwards instruction
                                           instructions
                                           filtered))
                      ((and (eqv? (fxlogand flags backward:kills-arg2)
                                  backward:kills-arg2)
                            (not (vector-ref registers
                                             (instruction.arg2 instruction))))
                       ; Suppress initialization of dead register.
                       (suppress-backwards instruction
                                           instructions
                                           filtered))
                      ((and (eqv? op $movereg)
                            (= (instruction.arg1 instruction)
                               (instruction.arg2 instruction)))
                       (backwards instructions filtered))
                      (else
                       (let ((filtered (cons instruction filtered)))
                         (if (eqv? (fxlogand flags backward:kills-arg1)
                                   backward:kills-arg1)
                             (vector-set! registers
                                          (instruction.arg1 instruction)
                                          #f))
                         (if (eqv? (fxlogand flags backward:kills-arg2)
                                   backward:kills-arg2)
                             (vector-set! registers
                                          (instruction.arg2 instruction)
                                          #f))
                         (if (eqv? (fxlogand flags backward:uses-arg1)
                                   backward:uses-arg1)
                             (vector-set! registers
                                          (instruction.arg1 instruction)
                                          #t))
                         (if (eqv? (fxlogand flags backward:uses-arg2)
                                   backward:uses-arg2)
                             (vector-set! registers
                                          (instruction.arg2 instruction)
                                          #t))
                         (if (eqv? (fxlogand flags backward:uses-arg3)
                                   backward:uses-arg3)
                             (vector-set! registers
                                          (instruction.arg3 instruction)
                                          #t))
                         (backwards instructions filtered)))))))
        
        ; Given a list of instructions in reverse order, whose first
        ; element is the last instruction of a basic block,
        ; and a filtered list of instructions in forward order,
        ; returns a filtered list of instructions in the correct order.
        
        (define (backwards0 instructions filtered)
          (if (null? instructions)
              filtered
              (let* ((instruction (car instructions))
                     (mnemonic (instruction.op instruction)))
                (cond ((or (eqv? mnemonic $.label)
                           (eqv? mnemonic $.proc)
                           (eqv? mnemonic $.cont)
                           (eqv? mnemonic $.align))
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
                       (let* ((live (instruction.arg2 instruction))
                              (n+1 (min *nregs* (+ live 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (subvector-fill! registers n+1 *nregs* #f)
                         (let ((instruction
                                ; FIXME
                                (list mnemonic
                                      (lookup-label
                                       (instruction.arg1 instruction))
                                      live)))
                           (backwards (cdr instructions)
                                      (cons instruction filtered)))))
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
                       (let* ((live (instruction.arg2 instruction))
                              (n+1 (min *nregs* (+ live 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (let ((instruction
                                ; FIXME
                                (list mnemonic
                                      (lookup-label
                                       (instruction.arg1 instruction))
                                      live)))
                           (backwards (cdr instructions)
                                      (cons instruction filtered)))))
                      (else (backwards instructions filtered))))))
        
        (define (suppress-forwards instruction instructions filtered)
          (if (issue-warnings)
              '(begin (display suppression-message)
                      (newline)))
          (forwards instructions filtered))
        
        (define (suppress-backwards instruction instructions filtered)
          (if (issue-warnings)
              '(begin (display suppression-message)
                      (newline)))
          (backwards instructions filtered))
        
        (define (local-optimization-error op)
          (error "Compiler bug: local optimization" op))
        
        (vector-fill! registers #f)
        (forwards instructions '())))))
