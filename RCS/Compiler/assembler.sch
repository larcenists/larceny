; -*- Scheme -*-
;
; Fifth pass of the Scheme 313 compiler:
;   assembly.
;
; $Id: assembler.scm,v 1.4 91/08/20 15:31:25 lth Exp Locker: lth $
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

(define (assemble-nested-lambda as source)
  (let ((nested-as (make-assembly-structure source)))
    (as-nested! as (cons nested-as (as-nested as)))
    (assemble1 nested-as (lambda (as) (cons (reverse! (as-code as))
					    (reverse! (as-constants as)))))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; Emits the bits contained in the bytevector bv.
; (bv is really a symbolic instruction for the target architecture; it 
;  is a list.)

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
            (begin (as-source! as (cdr source))
                   ((vector-ref assembly-table (caar source))
                    (car source)
                    as)
                   (loop)))))
    (loop)))

; At the end of the first pass, the finalizer is run. It calls the target
; assembler on all code vectors, and cleans up all constant vectors as well.

(define (assemble-finalize! as)

  (define (traverse-constvector constlist labels)
    (define l '())
    (list->vector
      (map (lambda (x)
	     (case (car x)
	       ((codevector)
		(let ((segment (assemble-codevector (cadr x) labels)))
		  (set! l (append (cdr segment) labels))
		  (list 'codevector (car segment))))
	       ((constantvector)
		(list 'constantvector (traverse-constvector (cadr x) l)))
	       ((data)
		x)
	       ((global)
		x)
	       (else
		(error 'assembler "Funky constant slot ~a" (car x)))))
	   constlist)))

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

(define listify? #t)

; Arbitrary mnemonics for instructions and pseudo-instructions.

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
(define $opx (make-mnemonic 'opx))               ; opx     prim,x
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

(define $cons 'cons)

(define $usual-integrable-procedures$
  `((zero? 1 zero? #f)
    (= 2 = #t)
    (< 2 < #t)
    (> 2 > #t)
    (<= 2 <= #t)
    (>= 2 >= #t)
    (+ 2 + #t)
    (- 2 - #t)
    (* 2 * #t)
    (,(string->symbol "1+") 1 ,(string->symbol "1+") #f)        ; MacScheme
    (,(string->symbol "1-") 1 ,(string->symbol "1-") #f)        ; MacScheme
    (null? 1 null? #f)
    (pair? 1 pair? #f)
    (cons 2 cons #f)
    (car 1 car #f)
    (cdr 1 cdr #f)
    (make-vector 2 make-vector #f)
    (vector-length 1 vector-length #f)
    (vector-ref 2 vector-ref #t)
    (vector-set! 3 vector-set! #f)
    (,(string->symbol "MAKE-CELL") 1 make-cell #f)
    (,(string->symbol "CELL-REF") 1 cell-ref #f)
    (,(string->symbol "CELL-SET!") 2 cell-set! #f)))

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (emit-label! as (make-label (operand1 instruction)))))

; Given a numeric label, prepend a Q and make it a symbol (the assembler is
; a little picky...)

(define (make-label q)
  (string->symbol (string-append
		   "Q"
		   (number->string q))))

(define new-label
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string->symbol (string-append "L" (number->string n))))))

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

; A hack to deal with the MacScheme macro expander's treatment of
; 1+ and 1-, and some peephole optimization.

(define-instruction $op1
  (lambda (instruction as)
    (cond ((eq? (operand1 instruction) (string->symbol "1+"))
	   (push-instruction as (list $opx '+ 1)))
	  ((eq? (operand1 instruction) (string->symbol "1-"))
	   (push-instruction as (list $opx '- 1)))
	  ((and (eq? (operand1 instruction) 'null?)
		(eq? (operand0 (next-instruction as)) $branchf))
	   (let ((i (next-instruction as)))
	     (consume-next-instruction! as)
	     (push-instruction as (list $optb2 'bfnull? (operand1 i)))))
	  ((and (eq? (operand1 instruction) 'zero?)
		(eq? (operand0 (next-instruction as)) $branchf))
	   (let ((i (next-instruction as)))
	     (consume-next-instruction! as)
	     (push-instruction as (list $optb2 'bfzero? (operand1 i)))))
	  ((and (eq? (operand1 instruction) 'pair?)
		(eq? (operand0 (next-instruction as)) $branchf))
	   (let ((i (next-instruction as)))
	     (consume-next-instruction! as)
	     (push-instruction as (list $optb2 'bfpair? (operand1 i)))))
	  (else
	   (list-instruction "op1" instruction)
	   (emit-primop0! as (operand1 instruction))))))

; ($op2 prim k)

(define-instruction $op2

  (let ((oplist '((= bf=) (< bf<) (> bf>) (<= bf<=) (>= bf>=))))

    (lambda (instruction as)
      (let ((op (assq (operand1 instruction) oplist)))
	(if (and op
		 (eq? (operand0 (next-instruction as)) $branchf))
	    (let ((i (next-instruction as)))
	      (consume-next-instruction! as)
	      (push-instruction as (list $optb3
					 (cadr op)
					 (operand2 instruction)
					 (operand1 i))))
	    (begin
	      (list-instruction "op2" instruction)
	      (emit-primop1! as
			     (operand1 instruction)
			     (regname (operand2 instruction)))))))))

; ($op3 prim k1 k2)

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-primop2! as
		   (operand1 instruction)
		   (regname (operand2 instruction))
		   (regname (operand3 instruction)))))

; ($opx prim k x)
; Questionable use of argreg2?

(define-instruction $opx
  (lambda (instruction as)
    (list-instruction "opx" instruction)
    (emit-constant->register as (operand2 instruction) $r.argreg2)
    (emit-primop1! as
		   (operand1 instruction)
		   $r.argreg2)))

; Test-and-branch-on-false; introduced by peephole optimization of
; constructions of the form
;   ($op1 test)
;   ($bfalse label)
; The name of the test has been changed to make it easier for the backend.
;
; ($optb2 test label)

(define-instruction $optb2
  (lambda (instruction as)
    (list-instruction "optb2" instruction)
    (emit-primop1! as
		   (operand1 instruction)
		   (operand2 instruction))))

(define-instruction $optb3
  (lambda (instruction as)
    (list-instruction "optb3" instruction)
    (emit-primop2! as
		   (operand1 instruction)
		   (regname (operand2 instruction))
		   (operand3 instruction))))

; ($const foo)

(define-instruction $const
  (lambda (instruction as)
    (let ((next (next-instruction as)))
      (cond ((= (operand0 next) $setreg)
	     (consume-next-instruction! as)
	     (list-instruction "const2reg" (list '()
						 (operand1 instruction)
						 (operand1 next)))
	     (emit-constant->register as
				      (operand1 instruction)
				      (regname (operand1 next))))
	    (else
	     (list-instruction "const" instruction)
	     (emit-constant->register as (operand1 instruction) $r.result))))))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-global->register! as
			    (emit-global as (operand1 instruction))
			    $r.result)))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-register->global! as
			    (emit-global as (operand1 instruction))
			    $r.result)))

(define-instruction $lambda
  (lambda (instruction as)
    (list-lambda-start instruction)
    (let ((segment (assemble-nested-lambda as (operand1 instruction))))
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
    (emit-lexes! (operand1 instruction) (operand2 instruction))))

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
    (list-instruction "restore" instruction)
    (emit-restore! as (operand1 instruction))))

(define-instruction $pop
  (lambda (instruction as)
    (list-instruction "pop" instruction)
    (emit-pop! as (operand1 instruction))))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-load! as (operand1 instruction) $r.result)))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-store! as $r.result (operand1 instruction))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-load! as (operand1 instruction) (regname (operand2 instruction)))))

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
    (emit-lexical! as (operand1 instruction) (operand2 instruction))))

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

(define-instruction $save
  (lambda (instruction as)
    (list-instruction "save" instruction)
    (emit-save! as (operand1 instruction) (operand2 instruction))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-setrtn! as (operand1 instruction))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-jump! (operand1 instruction) (operand2 instruction))))

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

; Helpers

(define **eof** (lambda (x) x))
(define **unspecified** (lambda (y) y))

(define (emit-constant->register as opd r)

  (define (fixnum-range? x)
    (and (>= x (- (expt 2 29)))
	 (<= x (- (expt 2 29) 1))))
	     
  (cond ((integer? opd)
	 (if (fixnum-range? opd)	
	     (emit-fixnum->register! as opd r)
	     (emit-const->register! as (emit-constant as opd) r)))
	((rational? opd)
	 (emit-const->register! as (emit-constant as opd) r))
;	((rectangular? opd)
;	 (emit-const->register! as (emit-constant as opd) r))
	((real? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((complex? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((string? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((char? opd)
	 (emit-immediate->register! as (char->immediate opd) r))
	((pair? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((boolean? opd)
	 (emit-immediate->register! as
				    (if (eq? opd #t)
					$imm.true
					$imm.false)
				    r))
	((null? opd)
	 (emit-immediate->register! as $imm.null r))
	((symbol? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((vector? opd)
	 (emit-const->register! as (emit-constant as opd) r))
	((eq? opd **unspecified**)
	 (emit-immediate->register! as $imm.unspecified r))
	((eq? opd **eof**)
	 (emit-immediate->register! as $imm.eof r))
	(else
	 (error 'assemble "Unknown datatype as arg to `$const'"))))

