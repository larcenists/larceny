; -*- Scheme -*-
;
; Fifth pass of the Scheme 313 compiler:
;   assembly.
;
; $Id: assembler.sch,v 1.1 1995/08/01 04:42:39 lth Exp lth $
;
; Parts of this code is Copyright 1991 Lightship Software, Incorporated.
;
; This is a the front end of a simple, table-driven assembler.
;
; The input to this pass is a list of symbolic
; MacScheme machine instructions and pseudo-instructions.
; Each symbolic MacScheme machine instruction or pseudo-instruction
; is a list whose car is a small non-negative fixnum that acts as
; the mnemonic for the instruction.  The rest of the list is
; interpreted as indicated by the mnemonic.
;
; The output is a pair consisting of machine code (a bytevector)
; and a constant vector.
;
; (Large chunks of the original code has been removed since it did not 
;  have a purpose in the current organization. The present program uses a 
;  three-pass scheme, in which the MacScheme instructions are first converted
;  to symbolic instructions for the target architecture, then assembled 
;  using an assembler for that architecture.)
;
; This assembler is table-driven, and may be customized to emit
; machine code for different target machines.  The table consists
; of a vector of procedures indexed by mnemonics.  Each procedure
; in the table should take two arguments: an assembly structure
; and a source instruction.
;
; The table can be changed by redefining the following procedure.

(define (assembly-table) $bytecode-assembly-table$)

; Compiler switches

(define enable-singlestep? #f) ; Single stepping switch
(define enable-peephole? #t)   ; peephole optimization switch
(define listify? #f)           ; produce listing
(define emit-undef-check? #t)  ; check references to globals

; The main entry point.

(define (assemble source)
  (assemble1 (make-assembly-structure source)
             (lambda (as)
	       (assemble-finalize! as))))

; The following procedures are to be called by table routines.
;
; The assembly source for nested lambda expressions should be
; assembled by calling this procedure.  This allows an inner
; lambda to refer to labels defined by outer lambdas.
;
; The name is the name of the procedure; it may be any data object
; meaningful to the runtime system.

(define (assemble-nested-lambda as source name)
  (let ((nested-as (make-assembly-structure source)))
    (as-nested! as (cons nested-as (as-nested as)))
    (emit-constant nested-as name)
    (assemble1 nested-as (lambda (as) (cons (reverse! (as-code as))
					    (reverse! (as-constants as)))))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define (operand4 x) (car (cddddr x)))

; Emits the bits contained in the bytevector bv.
; (bv is really a symbolic instruction for the target architecture; it 
;  is a list.)
;
; Machine-level peephole optimization would fit nicely about here.

(define (emit! as bv)
  (as-code! as (cons bv (as-code as))))

; Given a Scheme object with its loader tag (data, codevector, or global),
; returns an index into the constant vector for that constant. Things may
; be shared.

(define (emit-constant as x)
  (emit-data as (list 'data x)))

(define (emit-global as x)
  (emit-data as (list 'global x)))

(define (emit-codevector as x)
  (emit-data as (list 'codevector x)))

(define (emit-constantvector as x)
  (emit-data as (list 'constantvector x)))

; do it.

(define (emit-data as x)
  (let* ((constants (as-constants as))
         (y (member x constants)))
    (if y
        (length y)
        (begin (as-constants! as (cons x constants))
               (+ (length constants) 1)))))

; A variation on the above, for constants (tag == data) only.
;
; Guarantees that the constants will not share structure
; with any others, and will occupy consecutive positions
; in the constant vector.  Returns the index of the first
; constant.
;
; Is this right? Should not all things be tagged?
; [Nodody uses this.]

(define (emit-constants as x . rest)
  (let* ((constants (as-constants as))
         (i (+ (length constants) 1)))
    (as-constants! as
                   (append (reverse rest)
                           (cons (list 'data x) constants)))
    i))



; For peephole optimization.

(define (next-instruction as)
  (let ((source (as-source as)))
    (if (null? source)
        '(-1)
        (car source))))

(define (consume-next-instruction! as)
  (as-source! as (cdr (as-source as))))

(define (push-instruction as instruction)
  (as-source! as (cons instruction (as-source as))))

; get the n'th previously emitted instruction (0-based)

(define (previous-emitted-instruction n as)
  (let loop ((n n) (code (as-code as)))
    (if (zero? n)
	(if (not (null? code))
	    (car code)
	    '(-1))
	(loop (- n 1) (if (null? code) '() (cdr code))))))

(define (discard-previous-instruction! as)
  (let ((code (as-code as)))
    (if (not (null? code))
	(as-code! as (cdr code)))))


; The remaining procedures in this file are local to the assembler.

; An assembly structure is a vector consisting of
;
;    table          (a table of assembly routines)
;    source         (a list of symbolic instructions)
;    lc             (location counter; an integer)
;    code           (a list of bytevectors)
;    constants      (a list)
;    labels         (an alist of labels and values)
;    fixups         (an alist of locations, sizes, and labels or fixnums)
;    nested         (a list of assembly structures for nested lambdas)
;
; In fixups, labels are of the form (<L>) to distinguish them from fixnums.
;
; In the generic version, lc, labels, fixups, and nested are not used.

(define label? pair?)
(define label.value car)

(define (make-assembly-structure source)
  (vector (assembly-table)
          source
          0
          '()
          '()
          '()
          '()
          '()))

(define (as-table as)     (vector-ref as 0))
(define (as-source as)    (vector-ref as 1))
(define (as-lc as)        (vector-ref as 2))
(define (as-code as)      (vector-ref as 3))
(define (as-constants as) (vector-ref as 4))
(define (as-labels as)    (vector-ref as 5))
(define (as-fixups as)    (vector-ref as 6))
(define (as-nested as)    (vector-ref as 7))

(define (as-source! as x)    (vector-set! as 1 x))
(define (as-lc! as x)        (vector-set! as 2 x))
(define (as-code! as x)      (vector-set! as 3 x))
(define (as-constants! as x) (vector-set! as 4 x))
(define (as-labels! as x)    (vector-set! as 5 x))
(define (as-fixups! as x)    (vector-set! as 6 x))
(define (as-nested! as x)    (vector-set! as 7 x))

; The guts of the assembler.

(define (assemble1 as finalize)
  (let ((assembly-table (as-table as)))

    (define (loop)
      (let ((source (as-source as)))
        (if (null? source)
            (finalize as)
            (begin (if enable-singlestep?
		       (emit-singlestep! as (car source)))
		   (if enable-peephole?
		       (let peeploop ((src1 source))
			 (peep as)
			 (let ((src2 (as-source as)))
			   (if (not (eq? src1 src2))
			       (peeploop src2)))))
		   (let ((source (as-source as)))
		     (as-source! as (cdr source))
		     ((vector-ref assembly-table (caar source))
		      (car source)
		      as)
		     (loop))))))

    (loop)))

; At the end of the first pass, the finalizer is run. It calls the target
; assembler on all code vectors, and cleans up all constant vectors as well.

(define (assemble-finalize! as)

  ; Descend into a constant vector and assemble the nested code vectors.
  ; "constlist" is the constant vector in a tagged list form. "labels" is the
  ; collected symbol table (as an assoc list) of all outer procedures.
  ; The return value is an actual vector with each slot tagged.
  ;
  ; The traversal must be done breadth-first in order to know all labels for
  ; the nested procedures.

  (define (traverse-constvector constlist labels)

    ; Traverse constant list. Return pair of constant list and new symbol
    ; table.
    ; Due to the nature of labels, it is correct to keep passing in the 
    ; accumulated symbol table to procedures on the same level.

    (define (do-codevectors clist labels)
      (let ((new-clist
	     (map (lambda (x)
		    (case (car x)
		      ((codevector)
		       (let ((segment (assemble-codevector (cadr x) labels)))
			 (set! labels (cdr segment))
			 (list 'codevector (car segment))))
		      (else
		       x)))
		  clist)))
	(cons new-clist labels)))

    ; Descend into constant vectors. Return the constant list.

    (define (do-constvectors clist labels)
      (map (lambda (x)
	     (case (car x)
	       ((constantvector)
		(list 'constantvector (traverse-constvector (cadr x) labels)))
	       (else
		x)))
	   clist))

    ;
    
    (let ((s (do-codevectors constlist labels)))
      (list->vector (do-constvectors (car s) (cdr s)))))

  ; assemble-finalize!

  (let ((code  (reverse! (as-code as)))
	(const (reverse! (as-constants as))))
    (let ((segment (assemble-codevector code '())))
      (cons (car segment) (traverse-constvector const (cdr segment))))))

; Guts of bytecode-to-assembly-language translation.
;
; We generate lists of assembly instructions, which are later assembled to 
; code vectors. The assembly instruction generators are themselves hidden in
; some other file; the end result is that this file is (hopefully entirely)
; target-independent.

(define $bytecode-assembly-table$
  (make-vector
   64
   (lambda (instruction as)
     (error "Unrecognized mnemonic" instruction))))

(define (define-instruction i proc)
  (vector-set! $bytecode-assembly-table$ i proc)
  #t)

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 12 (string-length name)))
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

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (emit-label! as (make-asm-label (operand1 instruction)))))

; Given a numeric label, prepend a Q and make it a symbol (the assembler is
; a little picky...)

(define (make-asm-label q)
  (string->symbol (string-append
		   "Q"
		   (number->string q))))

(define new-label
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "L" (number->string n))))))

(define-instruction $.asm
  (lambda (instruction as)
    (list-instruction ".asm" instruction)
    (emit! as (cadr instruction))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)
    (emit-.proc! as)))

; no-op on Sparc

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)
    '()))

; no-op on Sparc

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    '()))

; Instructions.

; ($op1 prim)
; A hack to deal with the MacScheme macro expander's treatment of 1+ and 1-.
;
; Does push-instruction work with the peephole optimizer in place??

(define-instruction $op1
  (lambda (instruction as)
    (cond ((eq? (operand1 instruction) (string->symbol "1+"))
	   (push-instruction as (list $op2imm '+ 1)))
	  ((eq? (operand1 instruction) (string->symbol "1-"))
	   (push-instruction as (list $op2imm '- 1)))
	  (else
	   (list-instruction "op1" instruction)
	   (emit-primop0! as (operand1 instruction))))))

; ($op2 prim k)

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-primop1! as
		   (operand1 instruction)
		   (regname (operand2 instruction)))))

; ($op3 prim k1 k2)

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-primop2! as
		   (operand1 instruction)
		   (regname (operand2 instruction))
		   (regname (operand3 instruction)))))

; ($op2imm prim x)
; Questionable use of argreg2 here?
; This is sub-optimal in that it generates the constant into a register
; before operating on it; however, peephole optimization rewrites critical
; code to use other operations, so the problem is minor. 

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "opx" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop1! as
		   (operand1 instruction)
		   $r.argreg2)))

; ($const foo)

(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (emit-constant->register as (operand1 instruction) $r.result)))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-global->register! as
			    (emit-global as (operand1 instruction))
			    $r.result)))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-result-register->global! as
			    (emit-global as (operand1 instruction)))))

(define-instruction $lambda
  (lambda (instruction as)
    (list-lambda-start instruction)
    (let ((segment (assemble-nested-lambda as 
					   (operand1 instruction)
					   (operand3 instruction))))
      (list-lambda-end)
      (let ((code-offset  (emit-codevector as (car segment)))
	    (const-offset (emit-constantvector as (cdr segment))))
	(emit-lambda! as
		      code-offset
		      const-offset
		      (operand2 instruction)
		      (operand3 instruction))))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (emit-lexes! as (operand1 instruction)
		    (operand2 instruction)))) 

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (emit-args=! as (operand1 instruction))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (emit-args>=! as (operand1 instruction))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-invoke! as (operand1 instruction))))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))        ; @@ Will
        (begin                                          ; @@ Will
         (list-instruction "restore" instruction)
         (emit-restore! as (operand1 instruction))))))

; begin @@ Will
; The previous-instruction peephole optimization for return
; in gen-msi.sch generates incorrect code for
;
;    pop     n
;    return
;
; This peephole optimization patches that bug, and generates
; better code for the above sequence.  This peephole optimization
; cannot be turned off because it patches a bug in emit-return!.

;(define-instruction $pop
;  (lambda (instruction as)
;    (list-instruction "pop" instruction)
;    (emit-pop! as (operand1 instruction))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "pop" instruction)
         (let ((next (next-instruction as)))
           (if (eq? $return (operand0 next))
               (begin (list-instruction "return" next)
                      (consume-next-instruction! as)
                      (emit-pop! as (operand1 instruction) #t))
               (emit-pop! as (operand1 instruction) #f)))))))

; end @@ Will

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-load! as (operand1 instruction) $r.result)))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-store! as $r.result (operand1 instruction))))

; begin @@ Will
; The load instruction now takes the register to be loaded as its
; first operand, and the stack slot from which to load as the second.

;(define-instruction $load
;  (lambda (instruction as)
;    (list-instruction "load" instruction)
;    (emit-load! as (operand1 instruction) (regname (operand2 instruction)))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-load! as (operand2 instruction) (regname (operand1 instruction)))))

; end @@ Will

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-store! as (regname (operand1 instruction)) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (emit-lexical! as (operand1 instruction) (operand2 instruction))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-setlex! as (operand1 instruction) (operand2 instruction))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-register->register! as (regname (operand1 instruction)) $r.result)))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-register->register! as $r.result (regname (operand1 instruction)))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-register->register! as 
			      (regname (operand1 instruction))
			      (regname (operand2 instruction)))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-return! as)))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)
    (emit-nop! as)))

; begin @@ Will
; The save instruction now takes just one operand.
; A save instruction is typically followed by several store instructions,
; so initializing the new frame to zero is often partially redundant.
; A peephole optimization works nicely here.

;(define-instruction $save
;  (lambda (instruction as)
;    (list-instruction "save" instruction)
;    (emit-save! as (operand1 instruction) (operand2 instruction))))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "save" instruction)
         (let* ((n (operand1 instruction))
                (v (make-vector (+ n 1) #t)))
           (emit-save0! as n)
           (if enable-peephole?
               (let loop ((instruction (next-instruction as)))
                 (if (eq? $store (operand0 instruction))
                     (begin (list-instruction "store" instruction)
                            (emit-store! as
                                         (regname (operand1 instruction))
                                         (operand2 instruction))
                            (consume-next-instruction! as)
                            (vector-set! v (operand2 instruction) #f)
                            (loop (next-instruction as))))))
           (emit-save1! as v))))))

; end @@ Will

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-setrtn! as (operand1 instruction))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-apply! as
		 (regname (operand1 instruction))
		 (regname (operand2 instruction)))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-jump! as (operand1 instruction) (operand2 instruction))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-branch! as #f (operand1 instruction))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-branch! as #t (operand1 instruction))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-branchf! as (operand1 instruction))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Operations introduced by the peephole optimizer.
;
; General mnemonic key:
;
;   optbregN     operation test-and-branch with op1 in explicit reg
;   optbregNimm  ditto with op2 an immediate (rather than explicit reg)
;   dresopN      operation with explicit source and target registers
;   dresopNimm   ditto with op2 an immediate (rather than explicit reg)
;
; A register can always be the symbol RESULT. (eh?)


; ($optbreg1 test-op src1 label)
; Test register and branch if false to label.

(define-instruction $optbreg1
  (let ((names '((null? . internal:bfnull?)
		 (zero? . internal:bfzero?)
		 (pair? . internal:bfpair?))))
    (lambda (instruction as)
      (list-instruction "optbreg1" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to optbreg1: ~a" instruction)
	    (emit-primop.3arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)))))))

; ($optbreg2 test-op src1 src2 label)
; Test two registers and branch if false to label.

(define-instruction $optbreg2
   (let ((names '((<       . internal:bf<)
 		 (>       . internal:bf>)
 		 (<=      . internal:bf<=)
 		 (>=      . internal:bf>=)
 		 (=       . internal:bf=)
 		 (char=?  . internal:bfchar=?)
 		 (char<=? . internal:bfchar<=?)
 		 (char<?  . internal:bfchar<?)
 		 (char>=? . internal:bfchar>=?)
 		 (char>?  . internal:bfchar>?)
 		 (eq?     . internal:bfeq?))))
    (lambda (instruction as)
      (list-instruction "optb2reg" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to optbreg2: ~a" instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))
			       (operand4 instruction)))))))

; ($optbreg2imm test-op src1 imm label)
; Test register and immediate and branch if false to label.
;
; Cheats: loads constant into register, then does register-register case.
; Dubious use of argreg2?
 
(define-instruction $optbreg2imm
   (let ((names '((<       . internal:bf<imm)
 		 (>       . internal:bf>imm)
 		 (<=      . internal:bf<=imm)
 		 (>=      . internal:bf>=imm)
 		 (=       . internal:bf=imm)
 		 (char=?  . internal:bfchar=?imm)
 		 (char>=? . internal:bfchar>=?imm)
 		 (char>?  . internal:bfchar>?imm)
 		 (char<=? . internal:bfchar<=?imm)
 		 (char<?  . internal:bfchar<?imm)
		 (eq? . internal:bfeq?imm))))
    (lambda (instruction as)
      (list-instruction "optbreg2imm" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to optbreg2imm: ~a" instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)
			       (operand4 instruction)))))))

; ($dresop1 op src1 dest)
; Operate on register and put result in register.

(define-instruction $dresop1
  (let ((names `((car            . internal:car2reg)
		 (cdr            . internal:cdr2reg)
		 (,name:CELL-REF . internal:cellref2reg))))
    (lambda (instruction as)
      (list-instruction "dresop1" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to dresop1: ~a" instruction)
	    (emit-primop.3arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))))))))

; ($dresop2 op src1 src2 dest)
; Operate on two registers and put result in register.

(define-instruction $dresop2
  (let ((names '((+    . internal:+2reg)
		 (-    . internal:-2reg)
		 (eq?  . internal:eq2reg)
		 (cons . internal:cons2reg))))
    (lambda (instruction as)
      (list-instruction "dresop2" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to dresop2: ~a" instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (if (eq? (operand3 instruction) 'RESULT)
				   $r.result
				   (regname (operand3 instruction)))
			       (if (eq? (operand4 instruction) 'RESULT)
				   $r.result
				   (regname (operand4 instruction)))))))))

; ($dresop2imm op src1 imm dest)
; Operate on register and immediate and put result in register.

(define-instruction $dresop2imm
  (let ((names '((+ . internal:+imm2reg)
		 (- . internal:-imm2reg))))
    (lambda (instruction as)
      (list-instruction "dresop2imm" instruction)
      (let ((op (assq (operand1 instruction) names)))
	(if (not op)
	    (error "Assembler: invalid op to dresop2imm: ~a" instruction)
	    (emit-primop.4arg! as
			       (cdr op)
			       (if (eq? (operand2 instruction) 'RESULT)
				   $r.result
				   (regname (operand2 instruction)))
			       (operand3 instruction)
			       (if (eq? (operand4 instruction) 'RESULT)
				   $r.result
				   (regname (operand4 instruction)))))))))

; ($constreg const dest)
; Move constant directly to register.

(define-instruction $constreg
  (lambda (instruction as)
    (list-instruction "constreg" instruction)
    (emit-constant->register as
			     (operand1 instruction)
			     (regname (operand2 instruction)))))


; ($branchfreg reg label)
; branch to label if reg is false.

(define-instruction $branchfreg
  (lambda (instruction as)
    (list-instruction "branchfreg" instruction)
    (emit-branchfreg! as 
		      (regname (operand1 instruction))
		      (operand2 instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers

(define **eof** (lambda (x) x))

(define (emit-constant->register as opd r)

  (define (fixnum-range? x)
    (and (>= x (- (expt 2 29)))
	 (<= x (- (expt 2 29) 1))))

  (cond ((and (integer? opd) (exact? opd))
	 (if (fixnum-range? opd)	
	     (emit-fixnum->register! as opd r)
	     (emit-const->register! as (emit-constant as opd) r)))
	((boolean? opd)
	 (emit-immediate->register! as
				    (if (eq? opd #t)
					$imm.true
					$imm.false)
				    r))
	; is this correct?
	((eq? opd **eof**)
	 (emit-immediate->register! as $imm.eof r))
	((equal? opd hash-bang-unspecified)
	 (emit-immediate->register! as $imm.unspecified r))
	((null? opd)
	 (emit-immediate->register! as $imm.null r))
	((char? opd)
	 (emit-immediate->register! as (char->immediate opd) r))
	(else
	 (emit-const->register! as (emit-constant as opd) r))))

; Chez Scheme specific!!
; Could be made portable with a little bit of effort.

(define (emit-singlestep! as instr)
  (if (and (not (memq (operand0 instr)
                      (list $.label $.proc $.cont $.align)))
           (not (and (eq? (operand0 instr) $load)
                     (zero? (operand1 instr)))))
      (let ((p (open-output-string))
	    (f (= (car instr) $restore)))
	(display (if (= (car instr) $lambda)
		     (list 'lambda '(...) (caddr instr) (cadddr instr))
		     (car (readify-lap (list instr))))
		 p)
	(let* ((s (get-output-string p))
	       (o (emit-constant as s)))
	  (emit-singlestep-instr! as f 0 o)
;	  (display o) (display ": ") (display s) (newline)
	  (close-output-port p)))))

; eof
