; Opcode table.
;
; $Id$

(define $.linearize -1)
(define $.label 63)
(define $.proc 62)        ; entry point for procedure
(define $.cont 61)        ; return point
(define $.align 60)

(define make-mnemonic
  (let ((count 0))
    (lambda (ignored)
      (set! count (+ count 1))
      count)))

(define $op1 (make-mnemonic 'op1))               ; op      prim
(define $op2 (make-mnemonic 'op2))               ; op2     prim,k
(define $op3 (make-mnemonic 'op3))               ; op3     prim,k1,k2
(define $op2imm (make-mnemonic 'opx))            ; op2imm  prim,x
(define $const (make-mnemonic 'const))           ; const   x
(define $global (make-mnemonic 'global))         ; global  x
(define $setglbl (make-mnemonic 'setglbl))       ; setglbl x
(define $lexical (make-mnemonic 'lexical))       ; lexical m,n
(define $setlex (make-mnemonic 'setlex))         ; setlex  m,n
(define $stack (make-mnemonic 'stack))           ; stack   n
(define $setstk (make-mnemonic 'setstk))         ; setstk  n
(define $load (make-mnemonic 'load))             ; load    n,k
(define $store (make-mnemonic 'store))           ; store   k,n
(define $reg (make-mnemonic 'reg))               ; reg     k
(define $setreg (make-mnemonic 'setreg))         ; setreg  k
(define $movereg (make-mnemonic 'movereg))       ; movereg k1,k2
(define $lambda (make-mnemonic 'lambda))         ; lambda  x,n,doc
(define $lexes (make-mnemonic 'lexes))           ; lexes   n,doc
(define $args= (make-mnemonic 'args=))           ; args=   k
(define $args>= (make-mnemonic 'args>=))         ; args>=  k
(define $invoke (make-mnemonic 'invoke))         ; invoke  k
(define $save (make-mnemonic 'save))             ; save    L,k
(define $setrtn (make-mnemonic 'setrtn))         ; setrtn  L
(define $restore (make-mnemonic 'restore))       ; restore n
(define $pop (make-mnemonic 'pop))               ; pop     k
(define $return (make-mnemonic 'return))         ; return
(define $mvrtn (make-mnemonic 'mvrtn))           ; mvrtn
(define $apply (make-mnemonic 'apply))           ; apply
(define $nop (make-mnemonic 'nop))               ; nop
(define $jump (make-mnemonic 'jump))             ; jump    m,o
(define $skip (make-mnemonic 'skip))             ; skip    L    ;forward
(define $branch (make-mnemonic 'branch))         ; branch  L
(define $branchf (make-mnemonic 'branchf))       ; branchf L
(define $optb2 (make-mnemonic 'optb2))           ; optb2   prim,L
(define $optb3 (make-mnemonic 'optb3))           ; optb3   prim,x,L

