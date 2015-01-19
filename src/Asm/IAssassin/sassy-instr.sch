;;; NASM/i386 macros for the MacScheme instruction set.
;;;
;;; $Id: i386-instr.asm 2715 2005-12-14 22:27:51Z tov $
;;; 
;;; Sections in this file
;;;   - Section 1: handy macros
;;;   - Section 2: utility macros for MAL instruction definitions
;;;   - Section 3: MAL instruction definitions
;;;   - Section 4: MAL primitive operations (OP1, OP2, OP2IMM, OP3)
;;; 
;;; Conventions in the generated code:
;;;   - Labels defined within the a codevector are local to the
;;;     codevector they are defined in (using the NASM convention
;;;     for local labels).
;;;   - The above implies that the JUMP instruction must know the
;;;     name of the codevector it is jumping to.
;;;   - Constants are emitted with symbolic names when possible.
;;;   - Any address that may escape to Scheme code (codevector ptr,
;;;     return address) must be aligned on a 4-byte boundary.  For
;;;     millicode calls this means inserting NOPs following the call
;;;     instruction; millicode stubs fix up the return address.
;;; 
;;; Conventions in this file:
;;;   - No concrete register names may be used!  Use symbolic names
;;;     only, to ease reassignments later (my Pentium 3 seems to have
;;;     preference for working in some registers over others; some
;;;     investigation must be done)
;;; 
;;; Performance, code size, etc:
;;;   - Using the low byte of a register when possible (eg for
;;;     tag testing) reduces code size and does not appear to
;;;     impact performance.
;;;   - 'add r, -1' generates a 32-bit datum; 'sub' might be better
;;;     if you know your constant is negative.
;;;   - The exception handling code is large.  Can we shrink further,
;;;     eg by encoding registers with arguments as well as restart
;;;     address in a literal following the call point?  Must pack 
;;; 	very densely to fit all in 4 bytes, but that would be a major
;;;     win; 3 would be better still.  Use variable-length encoding? 
;;;     (The great primop cleanup should convert almost all of the
;;;     continuable exceptions to noncontinuable traps, which should
;;;     help a lot with both speed and space.)
;;;   - Generally search for OPTIMIZEME below
;;;
;;; Defines affecting the generated code:
;;;   UNSAFE_CODE        omit all type checks
;;;   UNSAFE_GLOBALS     omit undefined-checks on globals
;;;   INLINE_ALLOCATION  inline all allocation
;;;   INLINE_ASSIGNMENT  inline the write barrier (partially)

;; [Maybe (make-assembly-structure ...)]
(define current-sassy-assembly-structure 
  (make-parameter "current-sassy-assembly-structure" #f))

(define (fake-sassy-assembly-structure!)
  (current-sassy-assembly-structure 
   (make-assembly-structure '() (assembly-table) (make-user-data)))
  (unspecified))

(define (unsafe-globals)
  (unsafe-code))

(define wb-dest-address-arg #t)

(define-syntax define-sassy-instr
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY ...)
     (define (NAME ARGS ...) (seqlist BODY ...))))) ; do this for now...

;; Apply f to args and pull out the resulting list.
;; (Seems trivial now; but idea is to move all the dependencies on the
;;  list structure into this function.)
(define (do-sassy-instr f . args)
  (apply f args))

;; Has to be a macro since order of eval is undef'd in Scheme
(define-syntax seqlist 
  (syntax-rules (cond let quasiquote repeat-times)
    ((seqlist) 
     (list))
    ((seqlist (repeat-times n EXP) EXPS ...)
     (seqlist (let loop ((i n)) (cond ((not (zero? i)) EXP (loop (- i 1))))) EXPS ...))
    ((_ (cond (Q A ...) ...) EXPS ...)
     (append (mcond '() (Q (seqlist A ...)) ...) 
             (seqlist EXPS ...)))
    ((_ (let ((I E) ...) BODY ...) EXPS ...)
     (append (let ((I E) ...) (seqlist BODY ...)) 
             (seqlist EXPS ...)))
    ((_ (let LOOP-ID ((I E) ...) BODY ...) EXPS ...)
     (append (let LOOP-ID ((I E) ...) (seqlist BODY ...))
             (seqlist EXPS ...)))
    ;; Note in below two clauses, first uses CONS, second uses APPEND
    ((_ (quasiquote EXP1) EXPS ...)
     (cons (quasiquote EXP1) (seqlist EXPS ...)))
    ((_ EXP1 EXPS ...)
     (let ((v EXP1))
       (append v (seqlist EXPS ...))))))

(define-syntax mcond
  (syntax-rules (else)
    ((_ end) end)
    ((_ end (else A)) A)
    ((_ end (Q A) C ...) (if Q A (mcond end C ...)))))
     

;; ;; NAME : Operand ... -> [Listof SassyInstr]
;; (define-sassy-instr (NAME ARGS ...) BODY)

(define (g-reg n) 
  (case n 
    ((0) $g.reg0) ((1) $g.reg1) ((2) $g.reg2) ((3) $g.reg3)
    ((4) $g.reg4) ((5) $g.reg5) ((6) $g.reg6) ((7) $g.reg7)
    ;; Bleah, Lars' naming convention got weird with below regs
    ;; TODO change the $r to $g before I merge this in.
    ((8) $r.reg8) ((9) $r.reg9) ((10) $r.reg10) ((11) $r.reg11)
    ((12) $r.reg12) ((13) $r.reg13) ((14) $r.reg14) ((15) $r.reg15)
    ((16) $r.reg16) ((17) $r.reg17) ((18) $r.reg18) ((19) $r.reg19)
    ((20) $r.reg20) ((21) $r.reg21) ((22) $r.reg22) ((23) $r.reg23)
    ((24) $r.reg24) ((25) $r.reg25) ((26) $r.reg26) ((27) $r.reg27)
    ((28) $r.reg28) ((29) $r.reg29) ((30) $r.reg30) ((31) $r.reg31)
    (else (error 'g-reg (string-append " unknown sw register "
                                       (number->string n))))))
(define (reg n)
  (case n   
    ((eax ebx ecx edx edi esi esp ebp) n)
    ((1) $r.reg1) ((2) $r.reg2) ((3) $r.reg3) ((0) $r.reg0)
    (else (error 'reg (string-append " unknown register "
                                     (number->string n))))))

(define (reg-low n)
  (case n
    ((eax) 'al)
    ((ebx) 'bl)
    ((ecx) 'cl)
    ((edx) 'dl)
    ((1) $r.reg1.low) ((2) $r.reg2.low)
    (else (error 'reg (string-append " unknown low register "
                                     (number->string n))))))

;; *tries* to produce the low variant of hwreg
(define (try-low hwreg)
  (case hwreg
    ((eax)    'al)
    ((ebx)    'bl)
    ((ecx)    'cl)
    ((edx)    'dl)
    ((edi)    'edi)
    ((esi)    'esi)
    ((esp)    'esp)
    ((ebp)    'ebp)
    (else (error 'try-low hwreg " is not a symbolic hwreg..."))))
 
;; has-low? : RegSymbol -> Boolean
;; Returns true if hwreg has low variant (see try-low above)
(define (has-low? hwreg)
  (memq hwreg '(eax ebx ecx edx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Handy macros for this and that.  More fundamental stuff is
;;; defined in i386-machine.ah

(define (invert-cc cc)
  (let-syntax ((cases (syntax-rules ()
                        ((cases x (cc ncc) ...)
                         (case x
                           ((cc) 'ncc) ...
                           ((ncc) 'cc) ...)))))
    (cases cc (z nz) (e ne) (g ng) (l nl) (o no) (le g) (be a))))
    
(define (result-reg? n)      (or (eq? n $r.result)))
(define (hwreg_has_low r)    
  (case r
    ((eax ebx ecx edx) #t)
    ((edi esi esp ebp) #f)
    (else 
     (or (result-reg? r) (= r 1) (= r 2)))))
(define (is_hwreg n)         (or (result-reg? n) (<= 0 n 3)))
(define (fixnum n)           (arithmetic-shift n 2))
(define (roundup8 x)         (logand (+ x 7) (lognot 7)))
(define (words2bytes n)      (* n $bytewidth.wordsize))
(define (stkslot n)          `(& ,$r.cont ,(+ $stk.reg0 (words2bytes n))))
(define (framesize n)        (roundup8 (+ $bytewidth.wordsize $stk.overhead (words2bytes n))))
(define (recordedsize n)     (+ $stk.overhead (words2bytes n)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Utility macros for MAL instruction definitions

(define-sassy-instr (ia86.mcall fcn name)
  ;; `(comment -- ,name)
  `(call	(& ,$r.globals ,fcn))
  `(align ,$bytewidth.code-align))

;;; loadr targetreg, regno
;;; 	load HW register targetreg from VM register regno
	
(define-sassy-instr (ia86.loadr targetreg regno)
  (cond ((is_hwreg regno)
         `(mov ,targetreg ,(reg regno)))
        (else 
         `(mov ,targetreg (& ,$r.globals ,(g-reg regno))))))

;;; storer regno, sourcereg
;;;     store VM register regno from HW register sourcereg
;;;     Does not destroy sourcereg

(define-sassy-instr (ia86.storer regno sourcereg)
  (cond ((is_hwreg regno)
         `(mov	,(reg regno) ,sourcereg))
        (else
         `(mov  (& ,$r.globals ,(g-reg regno)) ,sourcereg))))

;;; loadc hwreg, slot
;;;	Load constant vector element 'slot' into hwreg

(define (intel-reg? r)
  (memq r '(eax ebx ecx edx edi esi esp ebp al  bl  cl  dl)))

(define-sassy-instr (assert-intel-reg hwreg)
  (cond ((not (intel-reg? hwreg))
         (error 'assert-intel-reg hwreg))))
	
(define-sassy-instr (ia86.loadc hwreg slot)
  (assert-intel-reg hwreg)
  `(mov	,hwreg (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.constvector)))
  `(mov	,hwreg (& ,hwreg ,(+ (- $tag.vector-tag) (words2bytes (+ slot 1))))))

;;; write_barrier r1 r2
;;;	Move values from hardware registers r1 and r2 to RESULT 
;;; 	and SECOND and perform a write barrier.  r1 and r2 may 
;;; 	be #f, in which case the value must already be in RESULT 
;;; 	and SECOND.
;;;
;;;     For INLINE_ASSIGNMENT, test the r2 value and skip the barrier
;;;     if the low bit is not 1.

(define-sassy-instr (ia86.write_barrier r1 r2)
  (cond
   (#f ; (inline-assignment)
    ;; Note that this code path is unsound in the presence of the
    ;; concurrent SATB marker unless you have a proof that you do not
    ;; need to preserve the overwritten slot in the object for r1.
    ;; That is why this code path is disabled.
    ;; 
    ;; However, there is an flag in the globals array that indicates
    ;; if the concurrent marker is active; I probably could reenable
    ;; this path if it also checked that flag too.
    (let ((l0 (fresh-label)))
      (cond (r2
             `(test	,r2 1)
             `(jz short ,l0)
             `(mov	,$r.second ,r2))
            (else
             `(test	,$r.second 1)
             `(jz short ,l0)))
      (cond (r1
             `(mov	,$r.result ,r1)))
      (ia86.mcall $m.partial-barrier 'partial-barrier)
      `(label ,l0)))
   (else 
    (cond (r1
           `(mov ,$r.result ,r1)))
    (cond (r2
           `(mov ,$r.second ,r2)))
    (ia86.mcall $m.full-barrier 'full-barrier))))
	
(define-sassy-instr (ia86.with-write-barrier r1 r2 instrs)
  (ia86.write_barrier r1 r2)
  instrs)

(define-syntax with-write-barrier 
  (syntax-rules ()
    ((_ (r1 r2) EXP EXPS ...)
     (ia86.with-write-barrier 
      r1 r2 
      (seqlist EXP EXPS ...)))))

(define-sassy-instr (ia86.mov/wb dst-mem-forms src-reg)
  (let ((r1 (cadr dst-mem-forms)) (r2 src-reg))
    (let ((r1 (if (eqv? r1 $r.result) #f r1)) 
          (r2 (if (eqv? r2 $r.second) #f r2)))
      (with-write-barrier (r1 r2) 
       `(mov ,dst-mem-forms ,src-reg)))))

;;; timer_check
;;;	decrement timer and take interrupt if zero

(define-sassy-instr (ia86.timer_check)
  (let ((l1 (fresh-label)))
    `(dec (dword (& ,$r.globals ,$g.timer)))
    `(jnz short ,l1)
    (ia86.mcall	$m.timer-exception 'timer-exception)
    `(label ,l1)))

;;; exception_noncontinuable excode
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is noncontinuable
;;;
;;;     The register known variously as $r.temp or $r.second
;;;     must contain a valid tagged pointer (or fixnum) when
;;;     the code generated here is executed.  See ticket #686.
	
(define-sassy-instr (ia86.exception_noncontinuable excode)
  ;; `(comment -- exception ,excode)
  `(call	(& ,$r.globals ,$m.exception))
  `(dwords	,excode))
		
;;; exception_continuable excode restart
;;;	Jump to exception handler with code without destroying
;;;	any registers; exception is continuable at the address
;;;	of the second argument.  Smallest code probably results
;;;	from just inserting a jump.
;;;
;;;	Moving the exception code to globals takes 8 bytes using
;;;		mov	dword [GLOBALS+offset] ,ex
;;;     even if offset is short.  Placing the exception code in two 
;;; 	bytes at the return address saves six bytes, though the 
;;; 	handler code must adjust the return address.
;;;
;;;	Important that M_EXCEPTION is at short offset from
;;;	globals, to save 3 bytes!  (It can be a negative offset.)
;;;
;;;     The register known variously as $r.temp or $r.second
;;;     must contain a valid tagged pointer (or fixnum) when
;;;     the code generated here is executed.  See ticket #686.

(define-sassy-instr (ia86.exception_continuable excode short? restart)
  ;; `(comment -- exception ,excode)
  `(call	(& ,$r.globals ,$m.exception))
  `(dwords	,excode)
  `(align	,$bytewidth.code-align)
  `(jmp	,@(if short? '(short) '(try-short)) ,restart))

;;; alloc
;;;	Given fixnum number of words in RESULT, allocate
;;;	a structure of that many bytes and leave a raw pointer
;;;	in RESULT.

(define-sassy-instr (ia86.alloc)
  (cond ((inline-allocation)
         (let ((l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l1)
	   `(mov ,$r.temp (& ,$r.globals ,$g.etop))
           `(lea ,$r.temp (& ,$r.temp ,$r.result 4)) ; allocate and round
           `(and ,$r.temp -8)                  ;  up to 8-byte boundary
           `(add ,$r.temp ,$sce.buffer)
	   `(cmp ,$r.temp ,$r.cont)
	   `(jbe short ,l2)
	   (ia86.mcall $m.morecore 'morecore)
	   `(jmp short ,l1)
           `(label ,l2)
           `(mov ,$r.result (& ,$r.globals ,$g.etop))
           `(sub ,$r.temp ,$sce.buffer)
	   `(mov (& ,$r.globals ,$g.etop) ,$r.temp)))
        (else
         (ia86.mcall $m.alloc 'alloc))))

;;; const2reg hwreg const
;;; Move a constant to a register *without changing the flags*

(define-sassy-instr (ia86.const2reg hwreg const)
  (assert-intel-reg hwreg)
  `(mov ,hwreg ,const))         ; 5 bytes

;;; const2regf hwreg const
;;; Move a constant to a register, possibly killing the flags
;;; Makes for smaller code size.

(define-sassy-instr (ia86.const2regf hwreg const)
  (assert-intel-reg hwreg)
  (cond ((= const 0)
	 `(xor ,hwreg ,hwreg))  ; 2 bytes
	((= const 1)
         `(xor ,hwreg ,hwreg) ; 3 bytes
         `(inc ,hwreg))
	((= const -1)
         `(xor ,hwreg ,hwreg) ; 3 bytes
         `(dec ,hwreg))
       ;; This seems to hurt performance more than it helps size.
       ;;((and (has-low? hwreg) (<= 0 const 255))
       ;; `(xor ,hwreg ,hwreg) ; 4 bytes
       ;; `(mov ,(try-low hwreg) ,const))
	(else
	 `(mov ,hwreg ,const)))); 5 bytes
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; MacScheme machine instruction set

(define-sassy-instr (ia86.t_align x)
  `(align	,x))

(define-sassy-instr (ia86.t_cont)
  `(sub ,$r.cont ,(+ 4 $stk.retaddr)))

(define-sassy-instr (ia86.t_label x)
  `(label ,x))

;;; CONST is broken up into two instructions depending on the
;;; type of the constant.  The argument to T_CONST_IMM is a bitpattern
;;; or symbolic constant name, to T_CONST_CONSTVECTOR it is the
;;; constant vector index.
	
(define-sassy-instr (ia86.t_const_imm x)
  (ia86.const2regf $r.result x))

(define-sassy-instr (ia86.t_const_constvector x)
  (ia86.loadc	$r.result x))

(define-sassy-instr (ia86.t_const_setreg_imm x regno)
  (cond ((is_hwreg regno)
         (ia86.const2regf (reg regno) x))
        (else
         (ia86.const2regf $r.temp x) ; this could go through RESULT if we liked.
         ;; (should we instead mov directly to the slot for the swreg?)
	 (ia86.storer regno $r.temp))))

(define-sassy-instr (ia86.t_const_setreg_constvector x regno)
  (cond ((is_hwreg regno)
         (ia86.loadc (reg regno) x))
        (else
	 (ia86.loadc $r.temp x)
	 (ia86.storer regno $r.temp))))

;;; OPTIMIZEME:	if #!undefined fit in a byte, then we could do a byte
;;; compare here, at least.  (Why does it not fit in a byte?)

(define-sassy-instr (ia86.t_global x)
  (ia86.t_global_setreg x $r.result))

(define-sassy-instr (ia86.t_global_setreg x hwreg)
  (assert-intel-reg hwreg)
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    `(label ,l0)
    (ia86.loadc $r.temp x)
    `(mov ,hwreg (& ,$r.temp ,(- $tag.pair-tag)))
    (cond ((not (unsafe-globals))
           `(cmp ,hwreg ,$imm.undefined)
           `(jne short ,l1)
           `(mov ,$r.result ,$r.temp)
           (ia86.mcall $m.global-ex 'global-ex)
           `(jmp short ,l0)
           `(label ,l1)))))

(define-sassy-instr (ia86.wb-addr-third prefix temp-reg mem)
  (cond (wb-dest-address-arg
         prefix
         `(lea ,temp-reg ,mem)
         `(mov (& ,$r.globals ,$g.third) ,temp-reg))))

(define-sassy-instr (ia86.t_setglbl x)
  `(mov	,$r.second ,$r.result)
  (ia86.wb-addr-third (ia86.loadc $r.result x)
                      $r.result
                      `(& ,$r.result ,(- $tag.pair-tag)))
  (ia86.loadc	$r.result x)
  (ia86.mov/wb	`(& ,$r.result ,(- $tag.pair-tag)) $r.second))

(define-sassy-instr (ia86.t_const_setglbl_imm x glbl)
  (ia86.const2regf $r.second x)
  (ia86.wb-addr-third (ia86.loadc $r.result glbl) 
                      $r.result 
                      `(& ,$r.result ,(- $tag.pair-tag)))
  (ia86.loadc	$r.result glbl)
  (ia86.mov/wb	`(& ,$r.result ,(- $tag.pair-tag)) $r.second))

(define-sassy-instr (ia86.t_const_setglbl_constvector x glbl)
  (ia86.t_const_constvector x)
  (ia86.t_setglbl glbl))

(define-sassy-instr (ia86.t_reg_setglbl regno x)
  (ia86.wb-addr-third (ia86.loadc $r.result x)
                      $r.result
                      `(& ,$r.result ,(- $tag.pair-tag)))
  (ia86.loadc	$r.result x)
  (ia86.mov/wb `(& ,$r.result ,(- $tag.pair-tag)) (reg regno)))

(define-sassy-instr (ia86.t_lexical rib off)
  (cond ((> rib 0)
         `(mov ,$r.temp (& ,$r.reg0 ,(+ $proc.reg0 (- $tag.procedure-tag))))
         (repeat-times (- rib 1) 
                       `(mov ,$r.temp (& ,$r.temp ,(+ $proc.reg0 (- $tag.procedure-tag)))))
         `(mov ,$r.result (& ,$r.temp ,(+ $proc.reg0 (- $tag.procedure-tag) (words2bytes off)))))
        (else
         `(mov ,$r.result (& ,$r.reg0 ,(+ $proc.reg0 (- $tag.procedure-tag) (words2bytes off)))))))

(define-sassy-instr (ia86.t_setlex rib off)
  (cond ((> rib 0)
         `(mov ,$r.temp (& ,$r.reg0 ,(+ $proc.reg0 (- $tag.procedure-tag))))
         (repeat-times (- rib 1) `(mov ,$r.temp (& ,$r.temp ,(+ $proc.reg0 (- $tag.procedure-tag)))))
         `(mov (& ,$r.temp ,(+ $proc.reg0 (- $tag.procedure-tag) (words2bytes off))) ,$r.result))
        (else
         `(mov (& ,$r.reg0 ,(+ $proc.reg0 (- $tag.procedure-tag) (words2bytes off))) ,$r.result))))
	
(define-sassy-instr (ia86.t_stack slot)
  `(mov	,$r.result ,(stkslot slot)))

(define-sassy-instr (ia86.t_setstk slot)
  `(mov	,(stkslot slot) ,$r.result))

(define-sassy-instr (ia86.t_load regno slot)
  (cond ((is_hwreg regno)
         `(mov ,(reg regno) ,(stkslot slot)))
        (else
         `(mov ,$r.temp ,(stkslot slot))
         (ia86.storer regno $r.temp))))

(define-sassy-instr (ia86.t_store regno slot)
  (cond ((is_hwreg regno)
         `(mov	,(stkslot slot) ,(reg regno)))
        (else
	 (ia86.loadr	$r.temp regno)
	 `(mov	,(stkslot slot) ,$r.temp))))

(define-sassy-instr (ia86.t_reg regno)
  (ia86.loadr $r.result regno))
	
;;; Does not destroy RESULT.  The peephole optimizer uses that fact.
(define-sassy-instr (ia86.t_setreg regno)
  (ia86.storer	regno $r.result))

(define-sassy-instr (ia86.t_movereg regno1 regno2)
  (cond ((is_hwreg regno1)
	 (ia86.storer regno2 (reg regno1)))
	((is_hwreg regno2)
	 (ia86.loadr (reg regno2) regno1))
	(else
         (ia86.loadr $r.temp regno1)
         (ia86.storer regno2 $r.temp))))

(define-sassy-instr (ia86.init_closure r)
  (let ((regno (cond ((> r *lastreg*)
                      (- *lastreg* 1))
                     (else 
                      r)))
        (l1 (fresh-label)))
    (cond 
     ((> r *lastreg*)
;;;   ;; Using $r.cont here is sketchy when it can alias esp
      `(mov (& ,$r.globals ,$g.stkp) ,$r.cont)     ; Need a working register!
      `(mov (& ,$r.globals ,$g.result) ,$r.result) ; Save for later
      `(add ,$r.result ,(+ $proc.reg0 (words2bytes *lastreg*)))
      (ia86.loadr $r.cont *lastreg*)
      `(label ,l1)
      `(mov ,$r.temp (& ,$r.cont ,(- $tag.pair-tag)))
      `(mov (& ,$r.result) ,$r.temp)
      `(add ,$r.result ,$bytewidth.wordsize)
      `(mov ,$r.cont (& ,$r.cont ,(+ (- $tag.pair-tag) $bytewidth.wordsize)))
      `(cmp ,$r.cont ,$imm.null)
      `(jne short ,l1)
      `(mov ,$r.cont (& ,$r.globals ,$g.stkp))
      `(mov ,$r.result (& ,$r.globals ,$g.result))))
    
    (let rep ((regno regno))
      (cond 
       ((>= regno 0)
        (cond ((is_hwreg regno)
               `(mov (& ,$r.result ,(+ $proc.reg0 (words2bytes regno)))
                     ,(reg regno)))
              (else
               (ia86.loadr $r.temp regno)
               `(mov (& ,$r.result ,(+ $proc.reg0 (words2bytes regno))) ,$r.temp)))
        (rep (- regno 1)))))

    `(add ,$r.result.low ,$tag.procedure-tag)))

(define-sassy-instr (ia86.t_lambda codevec constvec n)
  ;; arguments are codevector offset, constant vector offset, and n
  (ia86.const2regf $r.result (fixnum (+ $proc.header-words $proc.overhead-words n 1)))
  (ia86.alloc)
  `(mov (dword (& ,$r.result)) ,(logior
                             (arithmetic-shift (words2bytes (+ $proc.overhead-words n 1))
                                               8)
                             $hdr.procedure))
  `(push ,$r.reg0) ; REG0 temp ptr to constants (saves size + reload of TEMP)
  `(mov	,$r.reg0 (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.constvector)))
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.vector-tag) (words2bytes (+ codevec 1)))))
  `(mov	(dword (& ,$r.result ,$proc.codevector)) ,$r.temp)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.vector-tag) (words2bytes (+ constvec 1)))))
  `(pop ,$r.reg0)  ; restore REG0
  `(mov	(dword (& ,$r.result ,$proc.constvector      )) ,$r.temp)
  (ia86.init_closure n))

(define-sassy-instr (ia86.t_lexes n)
  ;; argument is n
  (ia86.const2regf $r.result (fixnum (+ $proc.header-words $proc.overhead-words n 1)))
  (ia86.alloc)
  `(mov	(dword (& ,$r.result)) ,(logior
                             (arithmetic-shift (words2bytes (+ $proc.overhead-words n 1))
                                               8)
                             $hdr.procedure))
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.codevector)))
  `(mov	(dword (& ,$r.result ,$proc.codevector)) ,$r.temp)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.constvector)))
  `(mov	(dword (& ,$r.result ,$proc.constvector)) ,$r.temp)
  (ia86.init_closure n))

(define-sassy-instr (ia86.t_argseq n)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(cmp	,$r.result ,(fixnum n))
           `(je short ,l1)
           `(mov (& ,$r.globals ,$g.second) ,(fixnum n))
           (ia86.mcall	$m.argc-ex 'argc-ex)
           `(jmp short	,l0)
           `(label ,l1)))))


(define-sassy-instr (ia86.t_argsge n)
  (ia86.const2regf $r.second (fixnum n))
  (cond ((and (not (unsafe-code))
              (> n 0))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(cmp ,$r.result ,$r.second)
           `(jge short ,l1)
           `(mov (& ,$r.globals ,$g.second) ,(fixnum n))
           (ia86.mcall $m.argc-ex 'argc-ex)
           `(jmp short ,l0)
           `(label ,l1))))
  (ia86.mcall $m.varargs 'varargs))

;;; (See sassy-invoke.sch for the T_INVOKE and T_GLOBAL_INVOKE
;;;  definitions that used to be here.)
	
;;; Allocate the frame but initialize only the basic slots
;;; and any pad words.  Leave TEMP clear at the end; this
;;; fact is used by T_SAVE1 below.

(define-sassy-instr (ia86.t_save0 n)
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    `(label ,l0)
    `(sub ,$r.cont ,(+ $sce.buffer (framesize n)))
    `(cmp ,$r.cont (& ,$r.globals ,$g.etop))
    `(jae short ,l1)
    `(add ,$r.cont ,(+ $sce.buffer (framesize n)))
    (ia86.mcall $m.stkoflow 'stkoflow)
    `(jmp short ,l0)
    `(label ,l1)
    `(add ,$r.cont ,$sce.buffer)
    `(mov (dword (& ,$r.cont ,$stk.contsize)) ,(recordedsize n))
    ;; Not necessary to store reg0 here, this is handled
    ;; explicitly by the generated code.
    `(xor	,$r.temp ,$r.temp)
    `(mov	(dword (& ,$r.cont ,$stk.retaddr)) ,$r.temp)
    (cond ((= (- (framesize n) (recordedsize n)) 8)
           ;; We have a pad word at the end -- clear it
           `(mov (dword ,(stkslot (+ n 1))) ,$r.temp)))))

(define-sassy-instr (ia86.t_check_save n)
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    `(label ,l0)
    `(mov ,$r.temp ,$r.cont)
    `(sub ,$r.temp ,(+ $sce.buffer (framesize n)))
    `(cmp ,$r.temp (& ,$r.globals ,$g.etop))
    `(jae short ,l1)
    (ia86.mcall $m.stkoflow 'stkoflow)
    `(jmp short ,l0)
    `(label ,l1)))

(define-sassy-instr (ia86.t_setup_save_stores n)
  `(xor	,$r.temp ,$r.temp)
  (cond ((= (- (framesize n) (recordedsize n)) 8)
         ;; We have a pad word at the end -- clear it
         `(push ,$r.temp))))

(define-sassy-instr (ia86.t_push_store regno)
  (cond ((is_hwreg regno)
         `(push ,(reg regno)))
        (else
         `(push (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_push_temp)
  `(push ,$r.temp))

(define-sassy-instr (ia86.t_finis_save_stores n)
  (let ((v (let ((v (make-vector 3)))
             (vector-set! v (quotient $stk.contsize 4) 
                          (let ((r (recordedsize n)))
                            ;; (push byte) sign-extends its argument
                            (if (>= r 128) `(dword ,r) `(byte ,r))))
             (vector-set! v (quotient $stk.retaddr 4)  $r.temp)
             (vector-set! v (quotient $stk.dynlink 4)  $r.temp)
             v)))
    `(push ,(vector-ref v 2))
    `(push ,(vector-ref v 1))
    `(push ,(vector-ref v 0))))

;;; Initialize the numbered slot to the value of TEMP,
;;; which was cleared by ia86.t_save0.

(define-sassy-instr (ia86.t_save1 n)
  `(mov	(dword ,(stkslot n)) ,$r.temp))

;;; T_SAVE may still be emitted by the assembler when peephole 
;;; optimization is disabled.

(define-sassy-instr (ia86.t_save n)
  `(t_save0 ,n)
  (let rep ((slotno 1))
    (cond ((<= slotno n)
           `(t_save1 ,slotno)
           (rep (+ slotno 1))))))

'(define-sassy-instr (ia86.t_setrtn lbl)
  ;;; This has not been optimized.  (19 bytes)
  ;;; Ryan points out that we could grab the base address 
  ;;; of the codevector via R0 instead of doing the call below.
  (let ((l1 (fresh-label)))
    `(call $eip) ;; puts $eip into first element of GLOBALS.
    `(label ,l1) ;; absolute name for $eip
    `(pop ,$r.temp)  ;; stash return address in ,TEMP
    `(sub ,$r.temp (reloc rel ,l1)) ;; adjust to point to base of segment
    `(add ,$r.temp (reloc rel ,lbl))  ;; adjust to point to lbl
    `(mov (& ,$r.cont ,$stk.retaddr) ,$r.temp)   ;; save in ret addr slot
    ))

;; Alternate version (15 bytes).  
;; (But SETRTN/INVOKE may be inspired by prior approach...)
(define-sassy-instr (ia86.t_setrtn lbl)
  `(mov ,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.codevector)))
  `(add ,$r.temp (reloc abs 
                    ,lbl
                    ,(+ (- $tag.bytevector-tag))))
  `(mov (& ,$r.cont ,$stk.retaddr) ,$r.temp))

(define-sassy-instr (ia86.t_restore n)
  (let rep ((slotno 0))
    (cond ((<= slotno n)
           (cond ((is_hwreg slotno)
                  `(mov ,(reg slotno) (dword ,(stkslot slotno))))
                 (else
                  `(mov ,$r.temp (dword ,(stkslot slotno)))
                  `(mov (& ,$r.globals ,(g-reg slotno)) ,$r.temp)))
           (rep (+ slotno 1))))))

(define-sassy-instr (ia86.t_pop n)
  `(add	,$r.cont ,(framesize n)))
	
(define-sassy-instr (ia86.t_popstk)
  (error 't_popstk "not implemented -- students only"))

(define-sassy-instr (ia86.t_return)
  `(ret))

;;; (See sassy-invoke.sch for the T_APPLY definition that used to be here.)
	
(define-sassy-instr (ia86.t_nop)
  `(nop)) ;; The interface doesn't actually support empty lists, I think...

;; A nasty trick to force code alignment *after* a particular instruction
;; without human intervention.
;; First assemble instr to figure out how large it is, and then feed that 
;; to an alignment directive.
;; We do not hand off expressions to sassy until after we've finished building the 
;; entire input, so we do not need to worry about whether sassy is re-entrant to
;; do this trick.
(define-sassy-instr (ia86.align-after instr)
  (let ((instr-len (bytevector-length (sassy-text-bytevector (sassy `((text ,instr)))))))
    `(align ,$bytewidth.code-align ,(- (modulo instr-len $bytewidth.wordsize)))
    `(,@instr)))

;;; JUMP: Arguments are levels, label, and name of the code vector
;;; into which we are jumping (an artifact of the labelling system
;;; of the Petit implementation; we can almost certainly remove it
;;; after we have a complete Sassy backend).

(define-sassy-instr (ia86.t_jump levels label name)
  (ia86.t_jump* levels label #f))

(define-sassy-instr (ia86.t_setrtn_jump levels label)
  (ia86.t_jump* levels label #t))

(define-sassy-instr (ia86.t_jump* levels label setrtn?)
  (ia86.timer_check)
  (let ((offset
	 (let loop ((obj (current-sassy-assembly-structure)))
	   (cond ((not obj) ;; Didn't find our label by following
		  ;; chain of structures; must be in current one
		  #f)
		 ((find-label obj label) => (lambda (entry) (cdr entry)))
		 (else (loop (as-parent obj)))))))
    (repeat-times levels `(mov ,$r.reg0 (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.reg0))))
    ;; Now REG0 holds the closure we're jumping into; calculate the
    ;; address as start of codevector plus offset from above.
    (cond (offset
           `(lea ,$r.temp (& ,$r.reg0 ,(- $tag.procedure-tag)))
           `(mov ,$r.temp (& ,$r.temp ,$proc.codevector))
           `(lea ,$r.temp (& ,$r.temp ,(+ (- $tag.bytevector-tag) offset)))
           (cond (setrtn?
                  `(add ,$r.cont 4)
                  (ia86.align-after 
                   `(call ,$r.temp)))
                 (else
                  `(jmp ,$r.temp))))
          (else
           ;; If offset is false, then this must be a label in the
           ;; code vector we are currently attempting to construct.
           ;; At least, that's Felix's impression has after a
           ;; discussion with Will on the semantics of MacScheme's
           ;; JUMP.  Therefore emit a jmp and let Sassy handle it.
           (cond (setrtn?
                  (let ((ign '(begin (display "UGH!!!  MUST BE A BETTER WAY!")
                                    (newline))))
                    ;; We just clobbered reg0 above, but the reasoning above
                    ;; implies that this is the same codevector, so its okay.
                    `(mov ,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.codevector)))
                    `(add ,$r.temp (reloc abs 
                                      ,(compiled-procedure 
                                        (current-sassy-assembly-structure) 
                                        label)
                                      ,(- $tag.bytevector-tag))))
                  `(add ,$r.cont 4)
                  (ia86.align-after 
                   `(call ,$r.temp)))
                 (else
                  `(jmp try-short 
                        ,(compiled-procedure 
                          (current-sassy-assembly-structure) 
                          label))))))))

;;; The MAL assembler translates BRANCH and BRANCHF to SKIP and SKIPF
;;; as appropriate to avoid timer checks.

(define-sassy-instr (ia86.t_skip lbl)
  `(jmp try-short	,lbl))

(define-sassy-instr (ia86.t_skipf lbl)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	try-short ,lbl))

(define-sassy-instr (ia86.t_branch lbl)
  `(dec	(dword (& ,$r.globals ,$g.timer)))
  `(jnz	try-short ,lbl)
  (ia86.mcall	$m.timer-exception 'timer-exception)
  `(jmp	try-short ,lbl))

(define-sassy-instr (ia86.t_branchf lbl)
  (ia86.timer_check)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	try-short ,lbl))

(define-sassy-instr (ia86.t_reg_branchf regno lbl a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(try-low (reg regno)) ,$imm.false)
  `(je	try-short ,lbl))

(define-sassy-instr (ia86.t_check w x y lbl)
  `(cmp	,$r.result.low ,$imm.false)
  `(je	try-short ,lbl))

;; Felix thinks we shouldn't be seeing these.  And yet we do.
(define-sassy-instr (ia86.t_reg_check regno lbl)
  `(cmp	,(try-low (reg regno)) ,$imm.false)
  `(je	try-short ,lbl))

;; Call handler for noncontinuable exception with RESULT,
;; SECOND, and THIRD defined.

(define-sassy-instr (ia86.t_trap w x y z)
	;; Order matters here, because SECOND is TEMP and
	;; may be destroyed by loading of THIRD
  (cond ((not (= w 0))
         (ia86.loadr $r.result w)))
  (cond ((not (= y 0))
	   ;; OPTIMIZEME: optimize for case when %3 is HW reg
	   ;; (this will however have almost no impact)
         (ia86.loadr	$r.temp y)
         `(mov (& ,$r.globals ,$g.third) ,$r.temp)))
  (cond ((not (= x 0))
         (ia86.loadr $r.second x))
        (else 
         `(xor ,$r.second ,$r.second)))
  (ia86.exception_noncontinuable z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Helper macros for primitive operations

;;; setcc cc
;;;	Set RESULT to true if jcc jumps.
;;; 
;;;	It is probably the case that SECOND is available here,
;;;	but I'm not using that fact yet.
	
;(define-sassy-macro (ia86.setcc cc)				; 11 bytes, no jump
;	set,cc	RESULT_LOW		; 2 bytes
;	and	RESULT, 1		; 3 bytes
;	shl	RESULT, 2		; 3 bytes
;	or	RESULT_LOW, TRUE_CONST	; 3 bytes
;)

;;; Jesse thinks that the above is buggy (should be or'ing with FALSE_CONST)

;;; TRUE=6
;;; FALSE=2

(define-sassy-instr (ia86.setcc/cmov hwreg cc) ; 11 bytes, kills TEMP
  (assert-intel-reg hwreg) 
  (let ((cmovcc  (string->symbol (string-append "cmov" (symbol->string cc)))))
    `(mov ,hwreg ,$imm.false)           ; 5 bytes
    `(lea ,$r.temp  (& ,hwreg 4))          ; 3 bytes
    `(,cmovcc ,hwreg ,$r.temp)))           ; 3 bytes

(define-sassy-instr (ia86.setcc/set hwreg cc)	; ~12 bytes
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(reg-low hwreg))          ; 3 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(shl ,hwreg 2)                     ; 3 bytes
    `(or ,(reg-low hwreg) ,$imm.false))); 3 bytes

(define-sassy-instr (ia86.setcc/set-2 hwreg cc)	; 13 bytes
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(reg-low hwreg))          ; 3 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(lea ,hwreg (& (* 4 ,hwreg) 2))))  ; 7 bytes

(define-sassy-instr (ia86.setcc/set-3 hwreg cc)	; 12 bytes, kills TEMP
  (assert-intel-reg hwreg) 
  (let ((setcc  (string->symbol (string-append "set" (symbol->string cc)))))
    `(,setcc ,(reg-low hwreg))          ; 3 bytes
    `(xor ,$r.temp ,$r.temp)                  ; 2 bytes
    `(and ,hwreg 1)                     ; 3 bytes
    `(lea ,hwreg                        ; 4 bytes
          (& (* 4 ,hwreg) ,$r.temp 2))))

(define-sassy-instr (ia86.setcc/jmp hwreg cc)	; ~10 bytes, jumps
  (assert-intel-reg hwreg)
  (let ((l1 (fresh-label))
        (jcc (string->symbol (string-append "j" (symbol->string cc)))))
    (ia86.const2reg hwreg $imm.true)	; 5 bytes
    `(,jcc short ,l1)			; 2 bytes
    `(sub	,(try-low hwreg) 4)	; 3 bytes (would be 2 if RESULT=eax)
    `(label ,l1)))

(define-sassy-instr (ia86.setcc hwreg cc)
  (cond 
   (#f
    (ia86.setcc/cmov hwreg cc))
   (#f ; (hwreg_has_low hwreg)
    (ia86.setcc/set  hwreg cc))
   (else
    (ia86.setcc/jmp  hwreg cc))
   ))


;;; double_tag_predicate ptrtag, hdr
;;;	Set RESULT to #t if RESULT has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise set RESULT
;;;	to #f.

(define-sassy-instr (ia86.double_tag_predicate ptrtag hdr)
  (ia86.double_tag_test $r.result ptrtag hdr)
  (ia86.setcc $r.result 'z))

;;; fixnum_test_temp_is_free reg
;;;	Test reg for fixnum-ness and clear zero flag if fixnum.  OK to
;;;	destroy TEMP.
	
(define-sassy-instr (ia86.fixnum_test_temp_is_free regno)
  (cond ((is_hwreg regno) 
         (cond 
          ((hwreg_has_low regno)
           `(test	,(reg-low regno) ,$tag.fixtagmask))
          (else
           ;; test	REG,reg, $tag.fixtagmask
           ;; Above is 6 bytes, below is 4 bytes.  Performance?
           `(mov	,$r.temp ,(reg regno))
           `(test	,$r.temp.low ,$tag.fixtagmask))))
        (else
         `(test	(byte (& ,$r.globals ,(g-reg regno))) ,$tag.fixtagmask))))

;;; single_tag_test ptrtag
;;;	Leave zero flag set if hwreg contains a value with the given
;;;     3-bit tag.
;;;
;;;     Can leave $r.temp ($r.second, TEMP, SECOND, eax) in invalid state.
;;;     See ticket #686.

(define-sassy-instr (ia86.single_tag_test hwreg x)
  (assert-intel-reg hwreg)
  `(lea	,$r.temp (& ,hwreg ,(- 8 x)))
  `(test	,$r.temp.low 7))
	
;;; single_tag_test_ex ptrtag, exception_code
;;;	Unless in unsafe mode, test the pointer in RESULT for the
;;;	tag and signal an exception if it does not match.

(define-sassy-instr (ia86.single_tag_test_ex hwreg x y)
  (assert-intel-reg hwreg)
  (cond 
   ((not (unsafe-code))
    (let ((l0 (fresh-label))
          (l1 (fresh-label)))
      `(label ,l0)
      (ia86.single_tag_test hwreg x)               ; puts garbage in $r.temp
      `(jz short ,l1)
      `(xor ,$r.temp ,$r.temp)                     ; see ticket #686
      (ia86.exception_continuable y 'short l0)
      `(label ,l1)))))

;;; double_tag_test ptrtag, hdr
;;;	Set the zero flag if hwreg has an object with tag ptrtag and
;;;     the object header has low byte == hdr, otherwise reset the flag.
;;;     If zero flag is set, leaves the header field in TEMP.

(define-sassy-instr (ia86.double_tag_test hwreg x y)
  (let ((l1 (fresh-label)))
    `(lea	,$r.temp (& ,hwreg ,(- x)))
    `(test	,$r.temp.low 7)
    `(jnz short ,l1)
    `(mov	,$r.temp (& ,$r.temp))
    `(cmp	,$r.temp.low ,y)
    `(label ,l1)))
	
;;; fixnum_arithmetic regno, operation, undo-operation, ex
;;;	
;;; The previous version of this code generator left an invalid pointer
;;; in $r.temp (aka $r.second, TEMP, SECOND, eax) when one of the
;;; operands is not a fixnum.  See ticket #686.
;;;
;;; FIXME: not tested because this appears to be semi-dead code.
;;; See src/Compiler/common.imp.sch.

(define-sassy-instr (ia86.fixnum_arithmetic regno y z ex)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.loadr $r.temp regno)
           `(test     ,$r.temp.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           `(test     ,$r.result.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           `(,y	,$r.result ,$r.temp)    ; perform operation
           `(jno short ,l2)
           `(,z	,$r.result ,$r.temp)    ; undo operation following overflow
           `(label ,l1)
           (ia86.exception_continuable ex 'short l0)	; second is tmp so 2nd arg is in place
           `(label ,l2)))
        ((is_hwreg regno)
         `(,y	,$r.result ,(reg regno)))
        (else
         `(,y	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.trusted_fixnum_compare sregno dregno regno y)
  (cond ((is_hwreg regno)
         `(cmp	,(reg sregno) ,(reg regno)))
        (else
         `(cmp	,(reg sregno) (& ,$r.globals ,(g-reg regno)))))
  (ia86.setcc	(reg dregno) y))

;;; fixnum_compare reg, cc, ex
;;; OPTIMIZEME for whenever ,x is a hwreg
;;;	
;;; The previous version of this code generator left an invalid pointer
;;; in $r.temp (aka $r.second, TEMP, SECOND, eax) when one of the
;;; operands is not a fixnum.  See ticket #686.
;;;
;;; FIXME: not tested because this appears to be semi-dead code.
;;; See src/Compiler/common.imp.sch.

(define-sassy-instr (ia86.fixnum_compare regno y z)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           (ia86.loadr $r.temp regno)
           `(test     ,$r.temp.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           `(test     ,$r.result.low ,$tag.fixtagmask)
           `(jz short ,l2)
           `(label ,l1)
           (ia86.exception_continuable z 'short l0)         ; second is tmp so 2nd arg is in place
           `(label ,l2 ))))
  (cond
   ((is_hwreg regno)
    `(cmp	,$r.result ,(reg regno)))
   (else
    `(cmp	,$r.result (& ,$r.globals ,(g-reg regno)))))
  (ia86.setcc $r.result y))

;;; fixnum_shift r2, operation, ex
;;; 
;;; Shift count must be in CL if it is not constant
;;; OPTIMIZEME: we can do better than what I do here.
	
(define-sassy-instr (ia86.fixnum_shift x y z)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.temp x)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           (ia86.loadr	$r.second x)            ; essential; see ticket #686
           `(jz short ,l2)
           `(label ,l1)
           (ia86.exception_continuable z 'short l0)
           `(label ,l2)
           `(cmp	,$r.temp ,(fixnum 32))	; SECOND is ,TEMP
           `(jge short ,l1)))
        (else
         (ia86.loadr	$r.temp x)))
  `(shr	,$r.temp 2)
  `(mov	(& ,$r.globals ,$g.regalias-ecx) ecx)
  `(mov	cl ,$r.temp.low)
  `(,y	,$r.result cl)
  `(mov	ecx (& ,$r.globals ,$g.regalias-ecx))
  (cond ((not (eq? y 'shl))
         ;; Right shifts: mask out low bits
         ;; OPTIMIZEME: if RESULT were eax, masking RESULT_LOW with a byte
         ;; would save one byte here.
         `(and	,$r.result ,(lognot $tag.fixtagmask)))))
	
;;; generic_arithmetic sregno, dregno, regno, operation, undo-operation, millicode
;;; Note that sregno and dregno are always hw registers
;;;
;;; Computes
;;;     dregno := sregno operation regno
;;;
;;; Special cases:
;;;     dregno is same as sregno
;;;         eliminates copy of sregno to dregno
;;;     regno is hardware register and distinct from dregno
;;;         eliminates copy of regno to TEMP except for millicode case
;;;     sregno is RESULT
;;;         eliminates copy to RESULT in millicode case
;;;     dregno is RESULT
;;;         eliminates copy to dregno in millicode case
	
(define-sassy-instr (ia86.generic_arithmetic sregno dregno regno y z millicode)
  (let ((l1 (fresh-label))
        (l2 (fresh-label))
        (defer-copy? (and (is_hwreg regno)
                          (not (equal? dregno regno)))))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,(reg sregno))
    `(test	,$r.temp.low ,$tag.fixtagmask)
    (cond ((not defer-copy?)
           (ia86.loadr	$r.temp regno)))
    `(jnz short ,l1)
    (cond ((not (equal? dregno sregno))
           `(mov ,(reg dregno) ,(reg sregno))))
    `(,y	,(reg dregno) ,(if defer-copy? (reg regno) $r.temp))
    `(jno short ,l2)
    `(,z	,(reg dregno) ,(if defer-copy? (reg regno) $r.temp))
    `(label ,l1)
    (cond (defer-copy?
           `(mov ,$r.temp ,(reg regno))))
    (cond ((not (result-reg? sregno))
           `(mov ,$r.result ,(reg sregno))))
    (ia86.mcall	millicode y)
    (cond ((not (result-reg? dregno))
           `(mov ,(reg dregno) ,$r.result)))
    `(label ,l2 )))

(define-sassy-instr (ia86.generic_compare_branchf hwreg regno jnc mcode l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,hwreg)
    `(test	,$r.temp.low ,$tag.fixtagmask)
    (ia86.loadr	$r.second regno)
    `(jz short ,l1)
    (cond ((not (result-reg? hwreg))
           `(mov ,$r.result ,hwreg)))
    (ia86.mcall	mcode jnc)
    `(cmp	,$r.result.low ,$imm.false)
    `(je try-short	,l)
    `(jmp short ,l2)
    `(label ,l1)
    `(cmp	,hwreg ,$r.second)
    `(,jnc try-short ,l)
    `(label ,l2)
    ))

;;; generic_compare reg, condition, millicode

(define-sassy-instr (ia86.generic_compare regno condition z)
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    (ia86.loadr	$r.temp regno)
    `(or	,$r.temp ,$r.result)
    `(test	,$r.temp.low ,$tag.fixtagmask)
    (ia86.loadr	$r.second regno)
    `(jz short ,l1)
    (ia86.mcall	z condition)
    `(jmp short ,l2	)
    `(label ,l1)
    `(cmp	,$r.result ,$r.second)
    (ia86.setcc	$r.result condition)
    `(label ,l2 )))

;;; generic_char_compare reg, cc, ex

(define-sassy-instr (ia86.generic_char_compare regno y z)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.second regno)
           `(cmp	,$r.second.low ,$imm.character)
           `(jz	short ,l2)
           `(label ,l1)
           (ia86.exception_continuable z 'short l0)
           `(label ,l2)
           `(cmp	,$r.result.low ,$imm.character)
           `(jne short ,l1)
           `(cmp	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(cmp	,$r.result ,(reg regno)))
        (else
         `(cmp	,$r.result (& ,$r.globals ,(g-reg regno)))))
  (ia86.setcc	$r.result y))

(define-sassy-instr (ia86.generic_imm_compare_branchf hwregno imm jnc mcode l)
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,(try-low (reg hwregno)) ,$tag.fixtagmask)
    `(jz short ,l1)
    (cond ((not (equal? $r.result hwregno))
           `(mov ,$r.result ,(reg hwregno))))
    (ia86.const2regf $r.second imm)
    (ia86.mcall	mcode jnc)
    `(cmp	,$r.result.low ,$imm.false)
    `(je try-short	,l)
    `(jmp short ,l2)
    `(label ,l1)
    `(cmp	,(reg hwregno) ,imm)
    `(,jnc try-short	,l)
    `(label ,l2)))

;;; generic_imm_compare imm, cc, millicode

(define-sassy-instr (ia86.generic_imm_compare imm cc mcode)
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,$r.result.low ,$tag.fixtagmask)
    `(jz short ,l1)
    (ia86.const2regf $r.second imm)
    (ia86.mcall	mcode cc)
    `(jmp short ,l2)
    `(label ,l1)
    `(cmp	,$r.result ,imm)
    (ia86.setcc	$r.result cc)
    `(label ,l2)))
	
;;; generic_char_imm_compare imm, cc, ex
	
(define-sassy-instr (ia86.generic_char_imm_compare x y z)
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,l0)
           `(cmp	,$r.result.low ,$imm.character)
           `(jz	short ,l1)
           (ia86.const2regf $r.second x)
           (ia86.exception_continuable z 'short l0)))
    `(label ,l1)
    `(cmp	,$r.result ,x)
    (ia86.setcc	$r.result y)))

;;; indexed_structure_length ptrtag, hdrtag, ex, byte?

(define-sassy-instr (ia86.indexed_structure_length/hdr ptrtag hdrtag ex byte?)		; string-length or bytevector-length
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           (ia86.double_tag_test $r.result ptrtag hdrtag)
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable ex 'short l0)
           `(label ,l1)
           `(mov	,$r.result ,$r.temp)))
        (else
         `(mov	,$r.result (& ,$r.result ,(- ptrtag)))))
  `(shr	,$r.result 8)
  (cond (byte?
         `(shl	,$r.result 2))))


;;; indexed_structure_length ptrtag, ex, byte?
	
(define-sassy-instr (ia86.indexed_structure_length ptrtag ex byte?)
  (ia86.single_tag_test_ex $r.result ptrtag ex)
  (ia86.indexed_structure_length_trusted ptrtag byte?))

(define-sassy-instr (ia86.indexed_structure_length_trusted ptrtag byte?)
  `(mov	,$r.result (& ,$r.result ,(- ptrtag)))
  `(shr	,$r.result 8)
  (cond (byte?
         `(shl	,$r.result 2))))


;;; indexed_structure_test reg_index, reg_value, ptrtag, hdrtag, ex, byte?, test_reg_value
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that reg_index is a fixnum in the range of the structure.
;;;	If hdrtag is zero then do not check it.

(define-sassy-instr (ia86.indexed_structure_test regno reg-value ptrtag hdrtag ex byte? test_reg_value)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label))
               (l3 (fresh-label)))
           `(label ,l0)
           (ia86.fixnum_test_temp_is_free regno)
           `(jnz short ,l1)
           (cond ((not (= hdrtag 0))
                  (ia86.double_tag_test $r.result ptrtag hdrtag)
                  `(jz short ,l2))
                 (else
                  (ia86.single_tag_test $r.result ptrtag)
                  `(jz short ,l3)))
           `(label ,l1)
	   (ia86.loadr  $r.temp  reg-value)
	   `(mov (& ,$r.globals ,$g.third) ,$r.temp)
           (ia86.loadr	$r.second regno)
           (ia86.exception_continuable ex 'short l0)
           (cond ((= hdrtag 0)
                  `(label ,l3)
                  `(mov	,$r.temp (& ,$r.result ,(- ptrtag)))))
           `(label ,l2)
           `(shr	,$r.temp 8)
           (cond (byte?
                  `(shl	,$r.temp 2)))	; Length is now a fixnum
           (cond ((is_hwreg regno)
                  `(cmp	,$r.temp ,(reg regno)))
                 (else
                  `(cmp	,$r.temp (& ,$r.globals ,(g-reg regno)))))
           `(jbe short ,l1)
           (test_reg_value	reg-value l1)))))

;;; indexed_structure_test_imm index, ptrtag, hdrtag, ex, byte?
;;;	Check that RESULT is a pointer tagged as appropriate,
;;;     and that index (a fixnum) is in the range of the structure.
;;;	If hdrtag is zero then do not check it.
	
(define-sassy-instr (ia86.indexed_structure_test_imm x y hdrtag ex byte?)
  (cond 
   ((not (unsafe-code))
    (let ((l0 (fresh-label))
          (l1 (fresh-label))
          (l2 (fresh-label))
          (l3 (fresh-label)))
      `(label ,l0)
      (cond ((not (= hdrtag 0))
             (ia86.double_tag_test $r.result y hdrtag)
             `(jz short ,l2))
            (else
             (ia86.single_tag_test $r.result y)
             `(jz short ,l3)))
      `(label ,l1)
      `(mov	,$r.second ,x)
      (ia86.exception_continuable ex 'short l0)
      (cond ((= hdrtag 0)
             `(label ,l3)
             `(mov	,$r.temp (& ,$r.result ,(- y)))))
      `(label ,l2)
      `(shr	,$r.temp 8)
      (cond (byte?
             `(shl	,$r.temp 2)))		; Length is now a fixnum
      `(cmp	,$r.temp ,x)
      `(jbe short ,l1)))))

;;; load_from_indexed_structure index_reg, ptrtag, byte?
	
(define-sassy-instr (ia86.load_from_indexed_structure regno y byte?)
  (cond (byte?
         (ia86.loadr	$r.temp regno)
         `(shr	,$r.temp 2)
         `(mov	,$r.result.low (& ,$r.result ,$r.temp ,(+ (- y) $bytewidth.wordsize)))
         `(and	,$r.result #xFF))
        ((is_hwreg regno)
         `(mov	,$r.result (& ,$r.result ,(reg regno) ,(+ (- y) $bytewidth.wordsize))))
        (else
         (ia86.loadr	$r.temp regno)
         `(mov	,$r.result (& ,$r.result ,$r.temp ,(+ (- y) $bytewidth.wordsize))))))

(define-sassy-instr (ia86.load_from_indexed_structure_imm x y byte?)
  (cond (byte?
         `(mov	,$r.result.low (& ,$r.result ,(+ (- y) $bytewidth.wordsize (quotient x 4))))
         `(and	,$r.result #xFF))
        (else
         `(mov	,$r.result (& ,$r.result ,(+ (- y) $bytewidth.wordsize x))))))
				
;;; indexed_structure_ref reg, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in ,RESULT.

(define-sassy-instr (ia86.indexed_structure_ref/hdr reg ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag  hdrtag  ex  byte? ia86.check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

;;; indexed_structure_ref_imm idx, ptrtag, hdrtag, ex, byte?
;;;	Leave the raw value in $r.result.

(define-sassy-instr (ia86.indexed_structure_ref_imm/hdr idx ptrtag hdrtag ex byte?)
  (ia86.indexed_structure_test_imm  idx  ptrtag  hdrtag  ex  byte?)
  (ia86.load_from_indexed_structure_imm  idx  ptrtag  byte?))
				
;;; indexed_structure_ref reg, ptrtag, ex, byte?
;;;	Leave the raw value in RESULT.

(define-sassy-instr (ia86.indexed_structure_ref reg ptrtag ex byte?)
  (ia86.indexed_structure_test  reg 0  ptrtag 0  ex  byte? ia86.check_nothing)
  (ia86.load_from_indexed_structure  reg  ptrtag  byte?))

(define-sassy-instr (ia86.indexed_structure_ref_imm reg ptrtag ex byte?)
  (ia86.indexed_structure_test_imm  reg  ptrtag 0  ex  byte?)
  (ia86.load_from_indexed_structure_imm  reg  ptrtag  byte?))

;;; check_nothing regno, label
;;;	Just a placeholder.

(define-sassy-instr (ia86.check_nothing regno y)
  )
	
;;; check_fixnum regno, label
;;;	Branch to label if regno does not hold a fixnum

(define-sassy-instr (ia86.check_fixnum regno y)
  (ia86.fixnum_test_temp_is_free regno)
  `(jnz short ,y))
	
;;; check_char regno, label
;;;	Branch to label if regno does not hold a char.
;;;	Leaves char in TEMP (even if it is in hwreg, the value must
;;;	be shifted anyway).

(define-sassy-instr (ia86.check_char regno y)
  (ia86.loadr	$r.temp regno)
  `(cmp	,$r.temp.low ,$imm.character)
  `(jnz short ,y))

;;; indexed_structure_set_* reg_idx, reg_value, ptrtag, hdrtag, ex
;;;	If hdrtag is 0 then do not check it.

(define-sassy-instr (ia86.indexed_structure_set_char regno reg-value ptrtag hdrtag ex)
  (cond 
   ((unsafe-code) 
    ;; ia86.check_char has this effect, but is not called by
    ;; ia86.indexed_structure_test in unsafe mode
    (ia86.loadr $r.temp reg-value))
   (else
    (ia86.indexed_structure_test regno reg-value ptrtag hdrtag ex #t ia86.check_char)))
;;;   ;; Using $r.cont here is sketchy when it can alias esp
  `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
  (ia86.loadr	$r.cont regno)
  `(shr	,$r.temp ,$bitwidth.char-shift)
  `(shr	,$r.cont 2)
  `(mov	(& ,$r.result ,$r.cont ,(+ (- ptrtag) $bytewidth.wordsize)) ,$r.temp.low)
  `(mov	,$r.cont (& ,$r.globals ,$g.stkp)))

(define-sassy-instr (ia86.indexed_structure_set_uchar regno reg-value ptrtag hdrtag ex)
  (cond 
   ((unsafe-code) 
    ;; ia86.check_char has this effect, but is not called by
    ;; ia86.indexed_structure_test in unsafe mode
    (ia86.loadr $r.temp reg-value))
   (else
    (ia86.indexed_structure_test regno reg-value ptrtag hdrtag ex #t ia86.check_char)))
;;;   ;; Using $r.cont here is sketchy when it can alias esp
  `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
  (ia86.loadr	$r.cont regno)
  `(mov	(& ,$r.result ,$r.cont ,(+ (- ptrtag) $bytewidth.wordsize)) ,$r.temp)
  `(mov	,$r.cont (& ,$r.globals ,$g.stkp)))

(define-sassy-instr (ia86.indexed_structure_set_byte regno1 regno2 z hdrtag ex)
  (ia86.indexed_structure_test regno1 regno2 z hdrtag ex #t ia86.check_fixnum)
  (ia86.indexed_structure_set_byte_trusted regno1 regno2 z))

(define-sassy-instr (ia86.indexed_structure_set_byte_trusted regno1 regno2 z)
;;;   ;; Using $r.cont here is sketchy when it can alias esp
  `(mov	(& ,$r.globals ,$g.stkp) ,$r.cont)
  (ia86.loadr	$r.cont regno1)
  `(shr	,$r.cont 2)
  (ia86.loadr	$r.temp regno2)
  `(shr	,$r.temp 2)
  `(mov	(& ,$r.result ,$r.cont ,(+ (- z) $bytewidth.wordsize)) ,$r.temp.low)
  `(mov	,$r.cont (& ,$r.globals ,$g.stkp)))

(define-sassy-instr (ia86.indexed_structure_set_word regno1 regno2 z hdrtag ex)
  (ia86.indexed_structure_test regno1 regno2 z hdrtag ex #f ia86.check_nothing)
  (ia86.do_indexed_structure_set_word $r.result regno1 regno2 z))

;; hwregno is either 'RESULT or a hardware regno.
;; FIXME: we should consistently use symbols or numbers for regs
(define-sassy-instr (ia86.do_indexed_structure_set_word hwregno regno1 regno2 z)
  (cond ((and (is_hwreg regno2) (is_hwreg regno1))
         (ia86.wb-addr-third 
          '()
          $r.temp
          `(& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize)))
         (ia86.mov/wb `(& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize)) (reg regno2)))
        ((is_hwreg regno2)
         (ia86.wb-addr-third
          (ia86.loadr $r.temp regno1)
          $r.temp
          `(& ,(reg hwregno) ,$r.temp ,(+ (- z) $bytewidth.wordsize)))
         (with-write-barrier ((reg hwregno) (reg regno2))
            (ia86.loadr	$r.temp regno1)
            `(mov	(& ,(reg hwregno) ,$r.temp ,(+ (- z) $bytewidth.wordsize)) ,(reg regno2))))
        ((is_hwreg regno1)
         (ia86.wb-addr-third 
          '()
          $r.temp
          `(& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize)))
         (ia86.loadr	$r.second regno2)
         (ia86.mov/wb `(& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize)) $r.second))
        (else ;; regno1 and regno2 are *both* non-hw
         (let ((freereg (if (eq? (reg hwregno) $r.reg2) $r.reg1 $r.reg2)))
           `(mov (& ,$r.globals ,$g.writetmp) ,freereg)
           (ia86.loadr $r.second regno2)
           (ia86.loadr freereg regno1)
           `(lea ,freereg (& ,(reg hwregno) ,freereg ,(+ (- z) $bytewidth.wordsize)))
           (cond (wb-dest-address-arg
                  `(mov (& ,$r.globals ,$g.third) ,freereg)))
           (with-write-barrier ((reg hwregno) #f)
             `(mov (& ,freereg) ,$r.second))
           `(mov ,freereg (& ,$r.globals ,$g.writetmp)))
         )))

;; Same as ia86.do_indexed_structure_set_word, but with no write barrier.
;; hwregno is either 'RESULT or a hardware regno.
;; FIXME: we should consistently use symbols or numbers for regs

(define-sassy-instr (ia86.do_indexed_structure_set_word:nwb
                     hwregno regno1 regno2 z)
  (cond ((and (is_hwreg regno2) (is_hwreg regno1))
         `(mov (& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize))
               ,(reg regno2)))
        ((is_hwreg regno2)
         (ia86.loadr	$r.temp regno1)
         `(mov (& ,(reg hwregno) ,$r.temp ,(+ (- z) $bytewidth.wordsize))
               ,(reg regno2)))
        ((is_hwreg regno1)
         (ia86.loadr	$r.second regno2)
         `(mov (& ,(reg hwregno) ,(reg regno1) ,(+ (- z) $bytewidth.wordsize))
               ,$r.second))
        (else
         (ia86.loadr	$r.second regno2)
;;;   ;; Using $r.cont here is sketchy when it can alias esp
         `(mov (& ,$r.globals ,$g.stkp) ,$r.cont)
         (ia86.loadr	$r.cont regno1)
         `(mov (& ,(reg hwregno) ,$r.cont ,(+ (- z) $bytewidth.wordsize))
               ,$r.second)
         `(mov ,$r.cont (& ,$r.globals ,$g.stkp)))))

;;; make_indexed_structure_word regno ptrtag hdrtag ex
;;;	Allocate a word structure with the length specified in RESULT
;;;	(fixnum number of entries).  If ,x is not #f, then initialize 
;;;	it with the contents of (reg ,x), otherwise with #!unspecified.
	
(define-sassy-instr (ia86.make_indexed_structure_word regno y z ex)
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(test	,$r.result ,(logior $tag.fixtagmask #x80000000))
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable ex 'short l0)
           `(label ,l1))))
  `(mov	(& ,$r.globals ,$g.alloctmp) ,$r.result)
  `(add	,$r.result ,$bytewidth.wordsize)
  (cond ((eqv? regno #f)
         `(mov	,$r.second ,$imm.unspecified))
        (else
         (ia86.loadr	$r.second regno)))
  (ia86.mcall	$m.alloci 'alloci)
  `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
  `(shl	,$r.temp 8)
  `(or	,$r.temp ,z)
  `(mov	(& ,$r.result) ,$r.temp)
  `(add	,$r.result ,y))

;;; make_indexed_structure_byte regno hdrtag ex
;;;	Allocate a byte structure with the length specified in RESULT
;;;     (fixnum number of bytes).  If regno is not #f, then (reg ,x) must
;;;     hold a char value to be used for initialization (a check is
;;;     performed that is a char).  If regno is #f, no initialization 
;;; 	is performed.
;;;
;;; FIXME: This may be nearly-dead code.  The char assumes flat1 strings.

(define-sassy-instr (ia86.make_indexed_structure_byte regno hdrtag ex)
  (let ((l0 (fresh-label))
        (l1 (fresh-label))
        (l2 (fresh-label)))
    (cond ((not (unsafe-code))
           ;; OPTIMIZEME (size): Unless allocation is inline,
           ;; the fixnum test can be moved into the millicode.
           ;; (As can the char test, I guess -- in fact, this whole
           ;; instruction is probably best moved into millicode)
           ;; OPTIMIZEME (speed): Both branches are mispredicted here.
           `(label ,l0)
           (cond (regno
                  (ia86.loadr	$r.second regno))
                 (else 
                  `(xor ,$r.second ,$r.second)))
           `(test	,$r.result ,(logior $tag.fixtagmask #x80000000))
           `(jz short ,l2)
           `(label ,l1)
           (ia86.exception_continuable ex 'short l0)
           `(label ,l2 )
           (cond (regno
                  `(cmp	,$r.second.low ,$imm.character)
                  `(jne	short ,l1)))))
    `(mov	(& ,$r.globals ,$g.alloctmp) ,$r.result)
    `(add	,$r.result ,(fixnum $bytewidth.wordsize))
    (ia86.mcall	$m.alloc-bv 'alloc-bv)
    (cond (regno
           (ia86.loadr	$r.temp regno)
           `(mov	(& ,$r.globals ,$g.regalias-ecx) ecx)
           `(mov	(& ,$r.globals ,$g.regalias-edi) edi)
           `(shr	,$r.temp ,$bitwidth.char-shift)	; char arg to byte value
           `(mov	ecx (& ,$r.globals ,$g.alloctmp))
           `(shr	ecx 2)		; byte count
           `(lea	edi (& ,$r.result 4))	; destination ptr
           `(cld)
           `(rep (stosb))		; byte fill
           `(mov	ecx (& ,$r.globals ,$g.regalias-ecx))
           `(mov	edi (& ,$r.globals ,$g.regalias-edi))))
    `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
    `(shl	,$r.temp 6)
    `(or	,$r.temp ,hdrtag)
    `(mov	(& ,$r.result)  ,$r.temp)
    `(add	,$r.result ,$tag.bytevector-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Primitive operations
;;; 
;;; The names 'n' in t_op1_n are the primcodes from the table in
;;; Compiler/standard-C.imp.sch (yes, that's right).

(define-sassy-instr (ia86.t_op1 x)   ;; YUCK eval!
  (let ((f (case x
             ((1 break) ia86.t_op1_1) 
             ((3 unspecified) ia86.t_op1_3) 
             ((4 undefined) ia86.t_op1_4) 
             ((5 eof-object) ia86.t_op1_5)
             ((6 enable-interrupts) ia86.t_op1_6) 
             ((7 disable-interrupts) ia86.t_op1_7) 
             ((8 typetag) ia86.t_op1_8) 
             ((9 not) ia86.t_op1_9)
             ((10 null?) ia86.t_op1_10) 
             ((11 pair?) ia86.t_op1_11) 
             ((12 eof-object?) ia86.t_op1_12) 
             ((13 port?) ia86.t_op1_13) 
             ((14 structure?) ia86.t_op1_14)
             ((15 car) ia86.t_op1_15) 
             ((16 cdr) ia86.t_op1_16) 
             ((17 symbol?) ia86.t_op1_17) 
             ((18 complex?) ia86.t_op1_18) 
             ((20 rational?) ia86.t_op1_20) 
             ((21 compnum?) ia86.t_op1_21) 
             ((22 integer?) ia86.t_op1_22) 
             ((23 fixnum?) ia86.t_op1_23) 
             ((24 flonum?) ia86.t_op1_24)
             ((25 exact?) ia86.t_op1_25) 
             ((26 inexact?) ia86.t_op1_26) 
             ((27 exact->inexact) ia86.t_op1_27) 
             ((28 inexact->exact) ia86.t_op1_28) 
             ((29 round) ia86.t_op1_29)
             ((30 truncate) ia86.t_op1_30) 
             ((31 zero?) ia86.t_op1_31) 
             ((32 --) ia86.t_op1_32) 
             ((33 fxlognot) ia86.t_op1_33) 
             ((34 real-part) ia86.t_op1_34)
             ((35 imag-part) ia86.t_op1_35) 
             ((36 char?) ia86.t_op1_36) 
             ((37 char->integer) ia86.t_op1_37) 
             ((char->integer:chr) ia86.t_op1_char->integer:chr)
             ((38 integer->char) ia86.t_op1_38) 
             ((integer->char:fix) ia86.t_op1_38) 
             ((integer->char:trusted) ia86.t_op1_integer->char:trusted)
             ((39 string?) ia86.t_op1_39)
             ((40 string-length string-length:str) ia86.t_op1_40) 
             ((   ustring?)       ia86.t_op1_ustring?)
             ((   ustring-length:str) ia86.t_op1_ustring-length:str)
             ((41 vector?) ia86.t_op1_41) 
             ((42 vector-length) ia86.t_op1_42) 
             ((43 bytevector?) ia86.t_op1_43) 
             ((44 bytevector-length) ia86.t_op1_44)
             ((46 make-bytevector) ia86.t_op1_46) 
             ((47 procedure?) ia86.t_op1_47) 
             ((48 procedure-length) ia86.t_op1_48) 
             ((49 make-procedure) ia86.t_op1_49)
             ((52 make-cell) ia86.t_op1_52) 
             ((54 cell-ref) ia86.t_op1_54)
             ((94 bytevector-like?) ia86.t_op1_94)
             ((95 vector-like?) ia86.t_op1_95)
             ((101 vector-like-length) ia86.t_op1_101) 
             ((102 bytevector-like-length) ia86.t_op1_102)
             ((    bytevector-like-length:bvl) ia86.t_op1_bvllength:bvl)
             ;((104 petit-patch-boot-code) ia86.t_op1_104)
             ((105 syscall) ia86.t_op1_105)
             ((106 creg) ia86.t_op1_106) 
             ((107 creg-set!) ia86.t_op1_107) 
             ((108 gc-counter) ia86.t_op1_108)
             ((109 major-gc-counter) ia86.t_op1_109)
             ((110 machine-address) ia86.t_op1_110)
             ((200 most-positive-fixnum) ia86.t_op1_200) 
             ((201 most-negative-fixnum) ia86.t_op1_201) 
             ((204 fx--) ia86.t_op1_204)
             ((211 fxzero?) ia86.t_op1_211) 
             ((212 fxpositive?) ia86.t_op1_212) 
             ((213 fxnegative?) ia86.t_op1_213)
             ((401 vector-length:vec) ia86.t_op1_401) 
             ((404 car:pair) ia86.t_op1_404)
             ((405 cdr:pair) ia86.t_op1_405)
             ((612 internal:branchf-zero?) ia86.t_op1_612)
             ((1000 timestamp!) ia86.t_op1_1000)
             ((1001 p-monitor!) ia86.t_op1_1001)
             (else (error 'ia86.t_op1 x))
             )))
    (f)))

;; t_op1* handles $reg/op1/setreg
(define-sassy-instr (ia86.t_op1* op rs rd)
  (let ((f (case op
             ((3 unspecified) ia86.t_op1_3*)
             ((4 undefined) ia86.t_op1_4*)
             ((5 eof-object) ia86.t_op1_5*)
             ((9 not) ia86.t_op1_9*)
             ((10 null?) ia86.t_op1_10*)
             ((11 pair?) ia86.t_op1_11*)
             ((12 eof-object?) ia86.t_op1_12*)
             ((15 car) ia86.t_op1_15*)
             ((16 cdr) ia86.t_op1_16*)
             ((23 fixnum?)    ia86.t_op1_23*)   ; FIXME: not being used
             ((47 procedure?) ia86.t_op1_47*)
             ((52 make-cell)  ia86.t_op1_52*)
             ((54 cell-ref)   ia86.t_op1_54*)
             ((94 bytevector-like?) ia86.t_op1_94*)
             ((95 vector-like?) ia86.t_op1_95*)
             ((401 vector-length:vec) ia86.t_op1_401*)
             ((404 car:pair) ia86.t_op1_404*)
             ((405 cdr:pair) ia86.t_op1_405*)
             (else (error 'ia86.t_op1* op))
             )))
    (f rs rd)))

(define-sassy-instr (ia86.t_op2 x y) ;; YUCK eval!
  (let ((f (case x
             ((45 bytevector-fill!) ia86.t_op2_45)
             ((55 typetag-set!) ia86.t_op2_55) 
             ((56 eq?) ia86.t_op2_56) 
             ((57 eqv?) ia86.t_op2_57) 
             ((58 cons) ia86.t_op2_58) 
             ((59 set-car!) ia86.t_op2_59)
             ((60 set-cdr!) ia86.t_op2_60) 
             ((61 +) ia86.t_op2_61) 
             ((62 -) ia86.t_op2_62) 
             ((63 *) ia86.t_op2_63) 
             ((64 /) ia86.t_op2_64)
             ((65 quotient) ia86.t_op2_65) 
             ((66 <) ia86.t_op2_66) 
             ((67 <=) ia86.t_op2_67) 
             ((68 =) ia86.t_op2_68) 
             ((69 >) ia86.t_op2_69)
             ((70 >=) ia86.t_op2_70) 
             ((71 fxlogand) ia86.t_op2_71) 
             ((72 fxlogior) ia86.t_op2_72) 
             ((73 fxlogxor) ia86.t_op2_73) 
             ((74 fxlsh) ia86.t_op2_74)
             ((75 fxrsha) ia86.t_op2_75) 
             ((76 fxrshl) ia86.t_op2_76) 
             ((77 rot) ia86.t_op2_77) 
             ((78 string-ref string-ref:trusted) ia86.t_op2_78) 
             ((79 string-set!) ia86.t_op2_79)
             ((   make-ustring)   ia86.t_op2_make-ustring)
             ((   ustring-ref:trusted) ia86.t_op2_ustring-ref:trusted)
             ((80 make-vector) ia86.t_op2_80) 
             ((81 vector-ref) ia86.t_op2_81) 
             ((82 bytevector-ref) ia86.t_op2_82) 
             ((83 procedure-ref) ia86.t_op2_83) 
             ((84 cell-set!) ia86.t_op2_84)
             ((   cell-set!:nwb) ia86.t_op2_84:nwb)
             ((85 char<?) ia86.t_op2_85) 
             ((86 char<=?) ia86.t_op2_86) 
             ((87 char=?) ia86.t_op2_87) 
             ((88 char>?) ia86.t_op2_88) 
             ((89 char>=?) ia86.t_op2_89)
             ((90 sys$partial-list->vector) ia86.t_op2_90)
             ((96 bytevector-like-ref) ia86.t_op2_96) 
             ((98 sys$bvlcmp) ia86.t_op2_98) 
             ((99 vector-like-ref) ia86.t_op2_99)
             ((103 remainder) ia86.t_op2_103) 
             ((109 make-string) ia86.t_op2_109)
             ((202 fx+) ia86.t_op2_202) 
             ((203 fx-) ia86.t_op2_203)
             ((205 fx*) ia86.t_op2_205) 
             ((206 fx=) ia86.t_op2_206) 
             ((207 fx<) ia86.t_op2_207) 
             ((208 fx<=) ia86.t_op2_208) 
             ((209 fx>) ia86.t_op2_209)
             ((210 fx>=) ia86.t_op2_210)
             ((402 vector-ref:trusted) ia86.t_op2_402) 
             ((406 =:fix:fix) ia86.t_op2_406) 
             ((407 <:fix:fix) ia86.t_op2_407) 
             ((408 <=:fix:fix) ia86.t_op2_408) 
             ((409 >=:fix:fix) ia86.t_op2_409)
             ((410 >:fix:fix) ia86.t_op2_410)
             ((500 +:idx:idx) ia86.t_op2_500) 
             ((501 +:fix:fix) ia86.t_op2_501) 
             ((502 -:idx:idx) ia86.t_op2_502) 
             ((503 -:fix:fix) ia86.t_op2_503)
             ((    bytevector-like-ref:trusted) ia86.t_op2_bvlref:trusted)
             ((=:flo:flo) ia86.t_op2_eq_floflo)
             ((<:flo:flo) ia86.t_op2_lt_floflo)
             ((<=:flo:flo) ia86.t_op2_lteq_floflo)
             ((>=:flo:flo) ia86.t_op2_gteq_floflo)
             ((>:flo:flo) ia86.t_op2_gt_floflo)
             ((+:flo:flo) ia86.t_op2_add_floflo)
             ((-:flo:flo) ia86.t_op2_sub_floflo)
             ((*:flo:flo) ia86.t_op2_mul_floflo)
             ((/:flo:flo) ia86.t_op2_div_floflo)
             (else (error 'ia86.t_op2 x))
             )))
    (f y)))

;; t_op2* handles $reg/op2/setreg

(define-sassy-instr (ia86.t_op2* op rs1 rs2 rd)
  (let ((f (case op
             ((56 eq?) ia86.t_op2_56*)
             ((58 cons) ia86.t_op2_58*)
             ((59 set-car!) ia86.t_op2_59*)
             ((60 set-cdr!) ia86.t_op2_60*)
             ((61 +) ia86.t_op2_61*)
             ((62 -) ia86.t_op2_62*)
             ((402 vector-ref:trusted) ia86.t_op2_402*)
             ((406 =:fix:fix) ia86.t_op2_406*)
             ((407 <:fix:fix) ia86.t_op2_407*)
             ((408 <=:fix:fix) ia86.t_op2_408*)
             ((409 >=:fix:fix) ia86.t_op2_409*)
             ((410 >:fix:fix) ia86.t_op2_410*)
             (else (error 'ia86.t_op2* op)))))
    ;; odd order of args is because of the odd define-sassy-instr/peep
    ;; interface (rs1 and rd are often $r.result)
    (f rs1 rd rs2)))

(define-sassy-instr (ia86.t_op2imm x y) ;; YUCK eval!
  (let ((f (case x
             ((128 typetag-set!) ia86.t_op2imm_128) 
             ((129 eq?) ia86.t_op2imm_129)
             ((130 +) ia86.t_op2imm_130) 
             ((131 -) ia86.t_op2imm_131) 
             ((132 <) ia86.t_op2imm_132) 
             ((133 <=) ia86.t_op2imm_133) 
             ((134 =) ia86.t_op2imm_134)
             ((135 >) ia86.t_op2imm_135) 
             ((136 >=) ia86.t_op2imm_136) 
             ((137 char<?) ia86.t_op2imm_137) 
             ((138 char<=?) ia86.t_op2imm_138) 
             ((139 char=?) ia86.t_op2imm_139)
             ((140 char>?) ia86.t_op2imm_140) 
             ((141 char>=?) ia86.t_op2imm_141) 
             ((142 string-ref) ia86.t_op2imm_142) 
            ; FIXME: should recognize immediate operands
            ;((    ustring-ref:trusted) ia86.t_op2imm_ustring-ref:trusted)
             ((143 vector-ref) ia86.t_op2imm_143) 
             ((144 bytevector-ref) ia86.t_op2imm_144)
             ((145 bytevector-like-ref) ia86.t_op2imm_145)
             ((146 vector-like-ref) ia86.t_op2imm_146)
             ((250 fx+) ia86.t_op2imm_250) 
             ((251 fx-) ia86.t_op2imm_251) 
             ((253 fx=) ia86.t_op2imm_253) 
             ((254 fx<) ia86.t_op2imm_254)
             ((255 fx<=) ia86.t_op2imm_255) 
             ((256 fx>) ia86.t_op2imm_256) 
             ((257 fx>=) ia86.t_op2imm_257)
             ((450 vector-ref:trusted) ia86.t_op2imm_450) 
             ((451 =:fix:fix) ia86.t_op2imm_451) 
             ((452 <:fix:fix) ia86.t_op2imm_452) 
             ((453 <=:fix:fix) ia86.t_op2imm_453) 
             ((454 >:fix:fix) ia86.t_op2imm_454)
             ((455 >=:fix:fix) ia86.t_op2imm_455)
             ((520 +:idx:idx) ia86.t_op2imm_520) 
             ((521 +:fix:fix) ia86.t_op2imm_521) 
             ((522 -:idx:idx) ia86.t_op2imm_522) 
             ((523 -:fix:fix) ia86.t_op2imm_523)
             ((    bytevector-like-ref:trusted) ia86.t_op2imm_bvlref:trusted)
             (else (error 'ia86.t_op2imm x))
             )))
    (f y)))

(define-sassy-instr (ia86.t_op2imm* op rs1 imm rd)
  (let ((f (case op
             ((129 eq?) ia86.t_op2imm_129*)
             ((130 +)   ia86.t_op2imm_130*)
             ((131 -)   ia86.t_op2imm_131*)
             ((450 vector-ref:trusted) ia86.t_op2imm_450*)
             ((451 =:fix:fix) ia86.t_op2imm_451*)
             ((452 <:fix:fix) ia86.t_op2imm_452*)
             ((453 <=:fix:fix) ia86.t_op2imm_453*)
             ((454 >:fix:fix) ia86.t_op2imm_454*)
             ((455 >=:fix:fix) ia86.t_op2imm_455*)
             ((520 +:idx:idx) ia86.t_op2imm_520*)
             ((521 +:fix:fix) ia86.t_op2imm_521*)
             ((522 -:idx:idx) ia86.t_op2imm_522*)
             ((523 -:fix:fix) ia86.t_op2imm_523*)
             (else (error 'ia86.t_op2imm* op)))))
    (f rs1 rd imm)))

(define-sassy-instr (ia86.t_op3 x y z) ;; YUCK eval!
  (let ((f (case x
             ((79 string-set! string-set!:trusted) ia86.t_op3_79)
             ((   ustring-set!:trusted) ia86.t_op3_ustring-set!:trusted)
             ((91 vector-set!) ia86.t_op3_91) 
             ((92 bytevector-set!) ia86.t_op3_92) 
             ((93 procedure-set!) ia86.t_op3_93) 
             ((97 bytevector-like-set!) ia86.t_op3_97)
             ((   bytevector-like-set!:trusted) ia86.t_op3_bvlset:trusted)
             ((100 vector-like-set!) ia86.t_op3_100)
             ((403 vector-set!:trusted) ia86.t_op3_403)
             ((vector-set!:trusted:nwb) ia86.t_op3_403:nwb)
             (else (error 'ia86.t_op3 x))
             )))
    (f y z)))

(define-sassy-instr (ia86.t_reg_op1_branchf op rs l a-skip?)
  (let ((f (case op
             ((internal:branchf-null?) ia86.t_reg_op1_branchf_null?)
             ((internal:branchf-eof-object?) ia86.t_reg_op1_branchf_eof_object?)
             ((internal:branchf-fixnum?) ia86.t_reg_op1_branchf_fixnum?)
             ((internal:branchf-flonum?) ia86.t_reg_op1_branchf_flonum?)
             ((internal:branchf-pair?) ia86.t_reg_op1_branchf_pair?)
             ((internal:branchf-vector?) ia86.t_reg_op1_branchf_vector?)
             ((internal:branchf-bytevector?) ia86.t_reg_op1_branchf_bytevector?)
             ((internal:branchf-structure?) ia86.t_reg_op1_branchf_structure?)
             ((internal:branchf-zero?) ia86.t_reg_op1_branchf_zero?)
             (else (error 'ia86.t_reg_op1_branchf op)))))
    (f rs l a-skip?)))

(define-sassy-instr (ia86.t_reg_op1_check op rs l)
  (let ((f (case op
             ((internal:check-fixnum?) ia86.t_reg_op1_check_fixnum?)
             ((internal:check-flonum?) ia86.t_reg_op1_check_flonum?)
             ((internal:check-pair?)   ia86.t_reg_op1_check_pair?)
             ((internal:check-vector?) ia86.t_reg_op1_check_vector?)
             ((internal:check-bytevector?) ia86.t_reg_op1_check_bytevector?)
             ((internal:check-string?) ia86.t_reg_op1_check_string?)
             ((internal:check-ustring?) ia86.t_reg_op1_check_ustring?)
             ((internal:check-structure?) ia86.t_reg_op1_check_structure?)
             (else (error 'ia86.t_reg_op1_check op)))))
    (f rs l)))

(define-sassy-instr (ia86.t_reg_op2_branchf op rs1 rs2 l a-skip?)
  (let ((f (case op
             ((internal:branchf-eq?) ia86.t_reg_op2_branchf_eq?)
             ((internal:branchf-<)   ia86.t_reg_op2_branchf_66)
             ((internal:branchf-<=)  ia86.t_reg_op2_branchf_67)
             ((internal:branchf-=)   ia86.t_reg_op2_branchf_68)
             ((internal:branchf->)   ia86.t_reg_op2_branchf_69)
             ((internal:branchf->=)  ia86.t_reg_op2_branchf_70)
             ((internal:branchf-=:fix:fix)   ia86.t_reg_op2_branchf_406)
             ((internal:branchf-<:fix:fix)   ia86.t_reg_op2_branchf_407)
             ((internal:branchf-<=:fix:fix)  ia86.t_reg_op2_branchf_408)
             ((internal:branchf->=:fix:fix)  ia86.t_reg_op2_branchf_409)
             ((internal:branchf->:fix:fix)   ia86.t_reg_op2_branchf_410)
             (else (error 'ia86.t_reg_op2_branchf op)))))
    (f rs1 rs2 l a-skip?)))

(define-sassy-instr (ia86.t_reg_op2imm_branchf op rs1 imm l a-skip?)
  (let ((f (case op
             ((internal:branchf-eq?/imm) ia86.t_reg_op2imm_branchf_eq?)
             ((internal:branchf-</imm)   ia86.t_reg_op2imm_branchf_132)
             ((internal:branchf-<=/imm)  ia86.t_reg_op2imm_branchf_133)
             ((internal:branchf-=/imm)   ia86.t_reg_op2imm_branchf_134)
             ((internal:branchf->/imm)   ia86.t_reg_op2imm_branchf_135)
             ((internal:branchf->=/imm)  ia86.t_reg_op2imm_branchf_136)
             ((internal:branchf-=:fix:fix/imm)   ia86.t_reg_op2imm_branchf_451)
             ((internal:branchf-<:fix:fix/imm)   ia86.t_reg_op2imm_branchf_452)
             ((internal:branchf-<=:fix:fix/imm)  ia86.t_reg_op2imm_branchf_453)
             ((internal:branchf->:fix:fix/imm)   ia86.t_reg_op2imm_branchf_454)
             ((internal:branchf->=:fix:fix/imm)  ia86.t_reg_op2imm_branchf_455)
             (else (error 'ia86.t_reg_op2imm_branchf op)))))
    (f rs1 imm l a-skip?)))

;; N.B. only rs1 is guaranteed to be a hwreg...
(define-sassy-instr (ia86.t_reg_op2_check op rs1 rs2 l)
  (let ((f (case op
             ((internal:check-=:fix:fix)  ia86.t_reg_op2_check_406)
             ((internal:check-<:fix:fix)  ia86.t_reg_op2_check_407)
             ((internal:check-<=:fix:fix) ia86.t_reg_op2_check_408)
             ((internal:check->=:fix:fix) ia86.t_reg_op2_check_409)
             ((internal:check->:fix:fix)  ia86.t_reg_op2_check_410)
             (else (error 'ia86.t_reg_op2_check op)))))
    (f rs1 rs2 l)))

(define-sassy-instr (ia86.t_reg_op2imm_check op rs1 imm l)
  (let ((f (case op
             ((internal:check-=:fix:fix)  ia86.t_reg_op2imm_check_451)
             ((internal:check-<:fix:fix)  ia86.t_reg_op2imm_check_452)
             ((internal:check-<=:fix:fix) ia86.t_reg_op2imm_check_453)
             ((internal:check->=:fix:fix) ia86.t_reg_op2imm_check_455)
             ((internal:check->:fix:fix)  ia86.t_reg_op2imm_check_454)
             (else (error 'ia86.t_reg_op2_check op)))))
    (f rs1 imm l)))

;; N.B. only rs1 is guaranteed to be a hwreg...
(define-sassy-instr (ia86.t_reg_op3 op rs1 rs2 rs3)
  (let ((f (case op
             ((internal:vector-set!:trusted) ia86.t_reg_op3_403)
             ((internal:vector-set!:trusted:nwb) ia86.t_reg_op3_403:nwb)
             (else (error 'ia86.t_reg_op3 op))
             )))
    (f rs1 rs2 rs3)))

(define-syntax define-sassy-instr/peep 
  (syntax-rules (or)
    ((_ (or (IA86.T_OPk_n* RS RD args-fst ...)
            (IA86.T_OPk_n  args ...))
        BODY ...)
     (begin
       (if (not (equal? '(args-fst ...) '(args ...)))
           ;; (safe guard against my own wee brain)
           (error 'define-sassy-instr/peep " ill-formed-args"))
       (define-sassy-instr (IA86.T_OPk_n args ...)
         (IA86.T_OPk_n* $r.result $r.result args ...))
       (define-sassy-instr (IA86.T_OPk_n* RS RD args ...)
         BODY ...)))))

(define-sassy-instr (ia86.t_op1_1)		; break
  (ia86.mcall	$m.break 'break))

(define-sassy-instr/peep (or (ia86.t_op1_3* rs rd)	; unspecified
                             (ia86.t_op1_3))
  (ia86.const2regf (reg rd) $imm.unspecified))

(define-sassy-instr/peep (or (ia86.t_op1_4* rs rd)	; undefined
                             (ia86.t_op1_4))
  (ia86.const2regf (reg rd) $imm.undefined))

(define-sassy-instr/peep (or (ia86.t_op1_5* rs rd)	; eof-object
                             (ia86.t_op1_5))
  (ia86.const2regf (reg rd) $imm.eof))

(define-sassy-instr (ia86.t_op1_6)		; enable-interrupts
  (ia86.mcall	$m.enable-interrupts 'enable-interrupts))

(define-sassy-instr (ia86.t_op1_7)		; disable-interrupts
  (ia86.mcall	$m.disable-interrupts 'disable-interrupts))

(define-sassy-instr (ia86.t_op1_8)		; typetag
  (ia86.mcall	$m.typetag 'typetag))

(define-sassy-instr/peep (or (ia86.t_op1_9* rs rd)		; not
                             (ia86.t_op1_9))
  `(cmp	,(try-low (reg rs)) ,$imm.false)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr/peep (or (ia86.t_op1_10* rs rd)		; null?
                             (ia86.t_op1_10))
  `(cmp	,(try-low (reg rs)) ,$imm.null)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr/peep (or (ia86.t_op1_11* rs rd)
                             (ia86.t_op1_11))		; pair?
  (ia86.single_tag_test (reg rs) $tag.pair-tag)
  (ia86.setcc	(reg rd) 'z))
	
(define-sassy-instr/peep (or (ia86.t_op1_12* rs rd)	; eof-object?
                             (ia86.t_op1_12))
  `(cmp	,(reg rs) ,$imm.eof)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr (ia86.t_op1_13)		; port?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.port))

(define-sassy-instr (ia86.t_op1_14)		; structure?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.struct))

(define-sassy-instr/peep (or (ia86.t_op1_15* rs rd)		; car
                             (ia86.t_op1_15))
  (ia86.single_tag_test_ex (reg rs) $tag.pair-tag $ex.car)
  `(mov	,(reg rd) (& ,(reg rs) ,(- $tag.pair-tag))))

(define-sassy-instr/peep (or (ia86.t_op1_16* rs rd)		; cdr
                             (ia86.t_op1_16))
  (ia86.single_tag_test_ex (reg rs) $tag.pair-tag $ex.cdr)
  `(mov	,(reg rd) (& ,(reg rs) ,(+ (- $tag.pair-tag) $bytewidth.wordsize))))

(define-sassy-instr (ia86.t_op1_17)		; symbol?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.symbol))

(define-sassy-instr (ia86.t_op1_18)		; number? and complex?
  (ia86.mcall	$m.complexp 'complexp))

(define-sassy-instr (ia86.t_op1_20)		; real? and rational?
  (ia86.mcall	$m.rationalp 'rationalp))

(define-sassy-instr (ia86.t_op1_21)		; compnum?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.compnum))

(define-sassy-instr (ia86.t_op1_22)		; integer?
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,$r.result.low ,$tag.fixtagmask)
    `(je short	,l1)
    (ia86.double_tag_test $r.result $tag.bytevector-tag $hdr.bignum)
    `(jz short ,l1)
    (ia86.mcall	$m.integerp 'integerp)
    `(jmp short ,l2)
    `(label ,l1)
    (ia86.const2regf $r.result $imm.true)
    `(label ,l2 )))

(define-sassy-instr/peep (or (ia86.t_op1_23* rs rd)		; fixnum?
                             (ia86.t_op1_23))
  (ia86.fixnum_test_temp_is_free rs)
  (ia86.setcc	(reg rd) 'z))
	
(define-sassy-instr (ia86.t_op1_24)		; flonum?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.flonum))

;(define-sassy-instr (ia86.t_op1_24)		; flonum?       ; FIXME
;  (ia86.mcall	$m.flonump 'flonump))

(define-sassy-instr (ia86.t_op1_25)		; exact?
  (ia86.mcall	$m.exactp 'exactp))

(define-sassy-instr (ia86.t_op1_26)		; inexact?
  (ia86.mcall	$m.inexactp 'inexactp))

(define-sassy-instr (ia86.t_op1_27)		; exact->inexact
  (ia86.mcall	$m.exact->inexact 'exact->inexact))

(define-sassy-instr (ia86.t_op1_28)		; inexact->exact
  (ia86.mcall	$m.inexact->exact 'inexact->exact))

(define-sassy-instr (ia86.t_op1_29)		; round
  (ia86.mcall	$m.round 'round))

(define-sassy-instr (ia86.t_op1_30)		; truncate
  (ia86.mcall	$m.truncate 'truncate))

(define-sassy-instr (ia86.t_op1_31)		; zero?
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,$r.result.low ,$tag.fixtagmask)
    `(jz short ,l1)
    (ia86.mcall	$m.zerop 'zerop)
    `(jmp short ,l2)
    `(label ,l1)
    `(and	,$r.result ,$r.result)
    (ia86.setcc	$r.result 'z)
    `(label ,l2)))

(define-sassy-instr (ia86.t_op1_32)		; --
  (ia86.mcall	$m.negate 'negate))

(define-sassy-instr (ia86.t_op1_33)		; fxlognot
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,l0)
           `(test	,$r.result.low ,$tag.fixtagmask)
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.lognot 'short l0)
           `(label ,l1)))
    `(lea	,$r.result (& ,$r.result ,$tag.fixtagmask))
    `(not	,$r.result)))

(define-sassy-instr (ia86.t_op1_34)		; real-part
  (ia86.mcall	$m.real-part 'real-part))
	
(define-sassy-instr (ia86.t_op1_35)		; imag-part
  (ia86.mcall	$m.imag-part 'imag-part))

(define-sassy-instr (ia86.t_op1_36)		; char?
  `(cmp	,$r.result.low ,$imm.character)
  (ia86.setcc	$r.result 'z))

(define-sassy-instr (ia86.t_op1_37)		; char->integer
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,l0)
           `(cmp	,$r.result.low ,$imm.character)
           `(jz	short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.char2int 'short l0)
           `(label ,l1)))
    (ia86.t_op1_char->integer:chr)))

(define-sassy-instr (ia86.t_op1_char->integer:chr) ; char->integer:chr
  `(shr	,$r.result 6))

(define-sassy-instr (ia86.t_op1_38)		; integer->char
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (fault (fresh-label)))
           `(label ,l0)
           ;; Argument must be fixnum
           `(test      ,$r.result.low ,$tag.fixtagmask)
           `(jnz short  ,fault)
           ;; Argument cannot be a surrogate (#x0000d800 - #x0000dfff).
           `(mov       ,$r.temp ,$r.result)
           `(sar       ,$r.temp 13)
           `(cmp       ,$r.temp #b11011)
           `(jz short  ,fault)
           ;; argument must be non-negative and less than #x00110000.
           `(cmp       ,$r.temp 544)
           `(jge short ,fault)
           `(cmp       ,$r.temp 0)
           `(jnl short ,l1)
           `(label ,fault)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.int2char 'short l0)
	   `(label ,l1))))
  (ia86.t_op1_integer->char:trusted))

(define-sassy-instr (ia86.t_op1_integer->char:trusted)	; integer->char:trusted
  `(shl	,$r.result 6)
  `(or	,$r.result.low ,$imm.character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The machine code to be generated for string primops
;;; depends on the representation of strings.
;;; The representations currently supported are flat4 and flat1.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-sassy-instr (flat1:string?)
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.string))

(define-sassy-instr (flat1:make-string regno)
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_byte regno $hdr.string  $ex.mkbvl))

(define-sassy-instr (flat1:string-length:str)
  (ia86.indexed_structure_length/hdr
   $tag.bytevector-tag $hdr.string $ex.slen #t))

(define-sassy-instr (flat1:string-ref:trusted regno)
  (ia86.indexed_structure_ref/hdr
   regno $tag.bytevector-tag  $hdr.string  $ex.sref #t)
  `(shl	,$r.result ,$bitwidth.char-shift)
  `(or	,$r.result.low ,$imm.character))

(define-sassy-instr (flat1:string-set!:trusted regno y)
  (ia86.indexed_structure_set_char
   regno y  $tag.bytevector-tag  $hdr.string  $ex.sset))

(define-sassy-instr (flat1:reg_op1_check_string? rs l)
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.string)
  `(jne try-short ,l))

(define-sassy-instr (flat4:string?)
  (ia86.double_tag_predicate
   $tag.bytevector-tag (+ $imm.bytevector-header $tag.ustring-typetag)))

(define-sassy-instr (flat4:make-string regno)
  ;; FIXME: exception code wrong, but matches Sparc
  (ia86.make_indexed_structure_word
       regno $tag.bytevector-tag
       (+ $imm.bytevector-header $tag.ustring-typetag)
       $ex.mkbvl))

(define-sassy-instr (flat4:string-length:str)
  `(mov	,$r.result (& ,$r.result ,(- $tag.bytevector-tag)))
  `(shr	,$r.result 8))

(define-sassy-instr (flat4:string-ref:trusted regno)
  (ia86.load_from_indexed_structure regno $tag.bytevector-tag #f))

(define-sassy-instr (flat4:string-set!:trusted regno y)
  (ia86.do_indexed_structure_set_word:nwb $r.result
                                          regno y $tag.bytevector-tag))

(define-sassy-instr (flat4:reg_op1_check_string? rs l)
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.ustring)
  `(jne try-short ,l))

(define ia86.t_op1_39                   flat1:string?)
(define ia86.t_op2_109                  flat1:make-string)
(define ia86.t_op1_40                   flat1:string-length:str)
(define ia86.t_op2_78                   flat1:string-ref:trusted)
(define ia86.t_op3_79                   flat1:string-set!:trusted)
(define ia86.t_reg_op1_check_string?    flat1:reg_op1_check_string?)
(define ia86.t_op1_ustring?             flat1:string?)
(define ia86.t_op2_make-ustring         flat1:make-string)
(define ia86.t_op1_ustring-length:str   flat1:string-length:str)
(define ia86.t_op2_ustring-ref:trusted  flat1:string-ref:trusted)
(define ia86.t_op3_ustring-set!:trusted flat1:string-set!:trusted)
(define ia86.t_reg_op1_check_ustring?   flat1:reg_op1_check_string?)

(let ((rep (nbuild-parameter 'target-string-rep)))
  (case rep
   ((flat1)
    (set! ia86.t_op1_39                   flat1:string?)
    (set! ia86.t_op2_109                  flat1:make-string)
    (set! ia86.t_op1_40                   flat1:string-length:str)
    (set! ia86.t_op2_78                   flat1:string-ref:trusted)
    (set! ia86.t_op3_79                   flat1:string-set!:trusted)
    (set! ia86.t_op1_ustring?             flat1:string?)
    (set! ia86.t_op2_make-ustring         flat1:make-string)
    (set! ia86.t_op1_ustring-length:str   flat1:string-length:str)
    (set! ia86.t_op2_ustring-ref:trusted  flat1:string-ref:trusted)
    (set! ia86.t_op3_ustring-set!:trusted flat1:string-set!:trusted)
    (set! ia86.t_reg_op1_check_ustring?   flat1:reg_op1_check_string?))
   ((flat4)
    (set! ia86.t_op1_39                   flat4:string?)
    (set! ia86.t_op2_109                  flat4:make-string)
    (set! ia86.t_op1_40                   flat4:string-length:str)
    (set! ia86.t_op2_78                   flat4:string-ref:trusted)
    (set! ia86.t_op3_79                   flat4:string-set!:trusted)
    (set! ia86.t_op1_ustring?             flat4:string?)
    (set! ia86.t_op2_make-ustring         flat4:make-string)
    (set! ia86.t_op1_ustring-length:str   flat4:string-length:str)
    (set! ia86.t_op2_ustring-ref:trusted  flat4:string-ref:trusted)
    (set! ia86.t_op3_ustring-set!:trusted flat4:string-set!:trusted)
    (set! ia86.t_reg_op1_check_ustring?   flat4:reg_op1_check_string?))
   (else
    (error "Unrecognized string representation: " rep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of string primops.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-sassy-instr (ia86.t_op1_41)		; vector?
  (ia86.double_tag_predicate $tag.vector-tag $hdr.vector))


(define-sassy-instr (ia86.t_op1_42)		; vector-length
  (ia86.indexed_structure_length/hdr $tag.vector-tag $hdr.vector $ex.vlen #f))
		
(define-sassy-instr (ia86.t_op1_43)		; bytevector?
  (ia86.double_tag_predicate $tag.bytevector-tag $hdr.bytevector))

(define-sassy-instr (ia86.t_op1_44)		; bytevector-length
  (ia86.indexed_structure_length/hdr $tag.bytevector-tag $hdr.bytevector $ex.bvlen #t))

;; OPTIMIZEME
(define-sassy-instr (ia86.t_op2_45 x)		; bytevector-fill!
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.single_tag_test $r.result $tag.bytevector-tag)
           `(jz short ,l2)
           `(label ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.bvfill 'short l0)
           `(label ,l2)
           (ia86.loadr	$r.second x)
           `(test	,$r.second.low ,$tag.fixtagmask)
           `(jnz short ,l1)))
        (else
         (ia86.loadr	$r.second x)))
  (ia86.mcall	$m.bytevector-like-fill 'bytevector-like-fill))

(define-sassy-instr (ia86.t_op1_46)		; make-bytevector
  (ia86.make_indexed_structure_byte #f $hdr.bytevector  $ex.mkbvl))

(define-sassy-instr/peep (or (ia86.t_op1_47* rs rd)		; procedure?
                             (ia86.t_op1_47))
  (ia86.single_tag_test (reg rs) $tag.procedure-tag)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr (ia86.t_op1_48)		; procedure-length
  (ia86.indexed_structure_length $tag.procedure-tag $ex.plen  #f))

(define-sassy-instr (ia86.t_op1_49)		; make-procedure
  ;; exception code wrong, matches Sparc
  (ia86.make_indexed_structure_word #f $tag.procedure-tag  $hdr.procedure  $ex.mkvl))
		
(define-sassy-instr/peep (or (ia86.t_op1_52* rs rd)	; make-cell just maps to cons, for now
                             (ia86.t_op1_52))
  (ia86.t_op2_58* rs rd 1)	; OPTIMIZEME: remove next instr by specializing
  `(mov	(& ,(reg rd) ,(- 4 $tag.pair-tag)) (dword ,$imm.unspecified)))

(define-sassy-instr/peep (or (ia86.t_op1_54* rs rd)	; cell-ref
                             (ia86.t_op1_54))
  `(mov	,(reg rd) (& ,(reg rs) ,(- $tag.pair-tag))))

(define-sassy-instr (ia86.t_op2_55 x)		; typetag-set!
  (ia86.loadr	$r.second x)
  (ia86.mcall	$m.typetag-set 'typetag-set))

(define-sassy-instr/peep (or (ia86.t_op2_56* rs1 rd rs2)       	; eq?
                             (ia86.t_op2_56 rs2))
  (cond ((is_hwreg rs2)
         `(cmp	,(reg rs1) ,(reg rs2)))
        (else
         `(cmp	,(reg rs1) (& ,$r.globals ,(g-reg rs2)))))
  (ia86.setcc	(reg rd) 'z))

;; XXX timer? XXX
(define-sassy-instr (ia86.t_reg_op2_branchf_eq? rs1 rs2 l a-skip?)
  (cond ((is_hwreg rs2)
         `(cmp	,(reg rs1) ,(reg rs2)))
        (else
         `(cmp	,(reg rs1) (& ,$r.globals ,(g-reg rs2)))))
  `(jne try-short ,l))

;; XXX timer? XXX
(define-sassy-instr (ia86.t_reg_op2imm_branchf_eq? rs1 imm l a-skip?)
  `(cmp	,(reg rs1) ,imm)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_op2_57 regno)		; eqv?
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.eqv 'eqv))

;; OPTIMIZEME: 42-46 bytes; can we do better?
(define-sassy-instr/peep (or (ia86.t_op2_58* rs1 rd rs2)       	; cons
                             (ia86.t_op2_58 rs2))
  (cond ((inline-allocation)
         (let ((l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l1)
           `(mov	,$r.temp (& ,$r.globals ,$g.etop))
           `(add	,$r.temp ,(+ $sce.buffer 8))
           `(cmp	,$r.temp ,$r.cont)
           `(jbe short ,l2)
           (ia86.mcall	$m.morecore 'morecore)
           `(jmp short ,l1)
           `(label ,l2)
           `(sub        ,$r.temp ,$sce.buffer)
           `(mov	(& ,$r.globals ,$g.etop) ,$r.temp)
           `(mov	(& ,$r.temp -8) ,(reg rs1))
           (cond ((is_hwreg rs2)
                  `(mov (& ,$r.temp -4) ,(reg rs2))
                  `(lea	,(reg rd) (& ,$r.temp ,(+ -8 $tag.pair-tag))))
                 (else
                  `(lea	,$r.result (& ,$r.temp ,(+ -8 $tag.pair-tag)))
                  (ia86.loadr	$r.temp rs2)
                  `(mov	(& ,$r.result ,(+ (- $tag.pair-tag) 4)) ,$r.temp)
                  (cond ((not (result-reg? rd))
                         `(mov ,(reg rd) ,$r.result)))))
           ))
        (else
         `(mov	(& ,$r.globals ,$g.alloctmp) ,(reg rs1))
         `(mov	,$r.result 8)
         (ia86.mcall	$m.alloc 'alloc)
         `(mov	,$r.temp (& ,$r.globals ,$g.alloctmp))
         `(mov	(& ,$r.result)  ,$r.temp)
         (cond ((is_hwreg rs2)
                `(mov	(& ,$r.result 4) ,(reg rs2)))
               (else
                (ia86.loadr	$r.temp rs2)
                `(mov	(& ,$r.result 4) ,$r.temp)))
         `(add	,$r.result ,$tag.pair-tag)
         (cond ((not (result-reg? rd))
                `(mov ,(reg rd) ,$r.result)))
         )))

(define (reg/result->num r)
  (cond ((result-reg? r) #f)
        ((number? r) r)
        (else (error 'reg/result->num r))))
	
(define-sassy-instr/peep (or (ia86.t_op2_59* rs1 rd rs2)	; set-car!
                             (ia86.t_op2_59 rs2))
  (ia86.single_tag_test_ex (reg rs1) $tag.pair-tag $ex.setcar)
  (ia86.wb-addr-third '() 
                      $r.temp
                      `(& ,(reg rs1) ,(- $tag.pair-tag)))
  (cond ((is_hwreg rs2)
         (ia86.mov/wb `(& ,(reg rs1) ,(- $tag.pair-tag)) (reg rs2)))
        (else
         (ia86.loadr	$r.second rs2)
         (ia86.mov/wb `(& ,(reg rs1) ,(- $tag.pair-tag)) $r.second))))

(define-sassy-instr/peep (or (ia86.t_op2_60* rs1 rd rs2)	; set-cdr!
                             (ia86.t_op2_60 rs2))
  (ia86.single_tag_test_ex (reg rs1) $tag.pair-tag $ex.setcdr)
  (ia86.wb-addr-third '()
                      $r.temp
                      `(& ,(reg rs1) 
                          ,(+ (- $tag.pair-tag) $bytewidth.wordsize)))
  (cond ((is_hwreg rs2)
         (ia86.mov/wb `(& ,(reg rs1) ,(+ (- $tag.pair-tag) $bytewidth.wordsize)) (reg rs2)))
        (else
         (ia86.loadr	$r.second rs2)
         (ia86.mov/wb `(& ,(reg rs1) ,(+ (- $tag.pair-tag) $bytewidth.wordsize)) $r.second))))

;;; If rs1 is not the same as rd, and rs2 is a hardware register,
;;; then we may get better code by reversing the arguments.

(define-sassy-instr/peep (or (ia86.t_op2_61* rs1 rd rs2) ; +
                             (ia86.t_op2_61 rs2))
  (cond ((equal? rd rs1)
         (ia86.generic_arithmetic rs1 rd rs2 'add 'sub $m.add))
        ((is_hwreg rs2)
         (ia86.generic_arithmetic rs2 rd rs1 'add 'sub $m.add))
        (else
         (ia86.generic_arithmetic rs1 rd rs2 'add 'sub $m.add))))

(define-sassy-instr/peep (or (ia86.t_op2_62* rs1 rd rs2) ; -
                             (ia86.t_op2_62 rs2))
  (ia86.generic_arithmetic rs1 rd rs2 'sub  'add  $m.subtract))

(define-sassy-instr (ia86.t_op2_63 regno)	; *
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.multiply 'multiply))
	
(define-sassy-instr (ia86.t_op2_64 regno)	; /
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.divide 'divide))

(define-sassy-instr (ia86.t_op2_65 regno)	; quotient
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.quotient 'quotient))

(define-sassy-instr (ia86.t_op2_66 regno)	; <
  (ia86.generic_compare regno 'l  $m.numlt))
	
(define-sassy-instr (ia86.t_op2_67 regno)	; <=
  (ia86.generic_compare regno 'le  $m.numle))

(define-sassy-instr (ia86.t_op2_68 regno)	; =
  (ia86.generic_compare regno 'e  $m.numeq))

(define-sassy-instr (ia86.t_op2_69 regno)	; >
  (ia86.generic_compare regno 'g  $m.numgt))

(define-sassy-instr (ia86.t_op2_70 regno)	; >=
  (ia86.generic_compare regno 'ge $m.numge))

(define-sassy-instr (ia86.t_reg_op2_branchf_66 rs1 rs2 l a-skip?)	; <
  (ia86.generic_compare_branchf (reg rs1) rs2 'jnl  $m.numlt l a-skip?))
	
(define-sassy-instr (ia86.t_reg_op2_branchf_67 rs1 rs2 l a-skip?)	; <=
  (ia86.generic_compare_branchf (reg rs1) rs2 'jg  $m.numle l a-skip?))

(define-sassy-instr (ia86.t_reg_op2_branchf_68 rs1 rs2 l a-skip?)	; =
  (ia86.generic_compare_branchf (reg rs1) rs2 'jne  $m.numeq l a-skip?))

(define-sassy-instr (ia86.t_reg_op2_branchf_69 rs1 rs2 l a-skip?)	; >
  (ia86.generic_compare_branchf (reg rs1) rs2 'jng  $m.numgt l a-skip?))

(define-sassy-instr (ia86.t_reg_op2_branchf_70 rs1 rs2 l a-skip?)	; >=
  (ia86.generic_compare_branchf (reg rs1) rs2 'jl $m.numge l a-skip?))


(define-sassy-instr (ia86.t_op2_71 regno)	; fxlogand
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           (ia86.loadr	$r.second regno)
           `(jz short ,l1)
           (ia86.exception_continuable $ex.logand 'short l0)
           `(label ,l1)
           `(and	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(and	,$r.result ,(reg regno)))
        (else
         `(and	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_72 regno)	; fxlogior
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           `(jz short ,l1)
           (ia86.loadr	$r.second regno)
           (ia86.exception_continuable $ex.logior 'short l0)
           `(label ,l1)
           `(mov	,$r.result ,$r.temp)))
        ((is_hwreg regno)
         `(or	,$r.result ,(reg regno)))
        (else
         `(or	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_73 regno)	; fxlogxor
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           (ia86.loadr	$r.second regno)
           `(jz short ,l1)
           (ia86.exception_continuable $ex.logxor 'short l0)
           `(label ,l1)	
           `(xor	,$r.result ,$r.second)))
        ((is_hwreg regno)
         `(xor	,$r.result ,(reg regno)))
        (else
         `(xor	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_74 regno)		; lsh
  (ia86.fixnum_shift regno 'shl  $ex.lsh))
	
(define-sassy-instr (ia86.t_op2_75 regno)		; rsha
  (ia86.fixnum_shift regno 'sar  $ex.rsha))

(define-sassy-instr (ia86.t_op2_76 regno)		; rshl
  (ia86.fixnum_shift regno 'shr  $ex.rshl))
	
(define-sassy-instr (ia86.t_op2_77 regno)		; rot
  (error 't_op2_rot "not implemented"))

(define-sassy-instr (ia86.t_op2_80 regno)		; make-vector
  (ia86.make_indexed_structure_word regno $tag.vector-tag  $hdr.vector  $ex.mkvl))

(define-sassy-instr (ia86.t_op2_81 regno)		; vector-ref
  (ia86.indexed_structure_ref/hdr regno $tag.vector-tag  $hdr.vector  $ex.vref #f))

(define-sassy-instr (ia86.t_op2_82 regno)		; bytevector-ref
  (ia86.indexed_structure_ref/hdr regno $tag.bytevector-tag  $hdr.bytevector  $ex.bvref #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.t_op2_83 regno)		; procedure-ref
  (ia86.indexed_structure_ref regno $tag.procedure-tag  $ex.pref #f))

(define-sassy-instr (ia86.t_op2_84 regno)		; cell-set!
  (ia86.wb-addr-third '()
                      $r.temp
                      `(& ,$r.result ,(- $tag.pair-tag)))
  (cond ((is_hwreg regno)
         (ia86.mov/wb `(& ,$r.result ,(- $tag.pair-tag)) (reg regno)))
        (else
         (ia86.loadr	$r.second regno)
         (ia86.mov/wb `(& ,$r.result ,(- $tag.pair-tag)) $r.second))))

(define-sassy-instr (ia86.t_op2_84:nwb regno)		; cell-set!:nwb
  (cond ((is_hwreg regno)
         `(mov (& ,$r.result ,(- $tag.pair-tag)) ,(reg regno)))
        (else
         (ia86.loadr	$r.second regno)
         `(mov (& ,$r.result ,(- $tag.pair-tag)) ,$r.second))))

(define-sassy-instr (ia86.t_op2_85 regno)		; char<?
  (ia86.generic_char_compare regno 'l  $ex.char<?))

(define-sassy-instr (ia86.t_op2_86 regno)		; char<=?
  (ia86.generic_char_compare regno 'le  $ex.char<=?))

(define-sassy-instr (ia86.t_op2_87 regno)		; char=?
  (ia86.generic_char_compare regno 'e  $ex.char=?))

(define-sassy-instr (ia86.t_op2_88 regno)		; char>?
  (ia86.generic_char_compare regno 'g  $ex.char>?))

(define-sassy-instr (ia86.t_op2_89 regno)		; char>=?
  (ia86.generic_char_compare regno 'ge  $ex.char>=?))

(define-sassy-instr (ia86.t_op2_90 regno)		; sys$partial-list->vector
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.partial-list->vector 'partial-list->vector))

(define-sassy-instr (ia86.t_op3_91 regno y)		; vector-set!
  (ia86.indexed_structure_set_word regno y  $tag.vector-tag  $hdr.vector  $ex.vset))

(define-sassy-instr (ia86.t_op3_92 regno y)		; bytevector-set!
  (ia86.indexed_structure_set_byte regno y  $tag.bytevector-tag  $hdr.bytevector  $ex.bvset))

(define-sassy-instr (ia86.t_op3_93 regno y)		; procedure-set!
  (ia86.indexed_structure_set_word regno y  $tag.procedure-tag  0  $ex.pset))

(define-sassy-instr/peep (or (ia86.t_op1_94* rs rd)		; bytevector-like?
                             (ia86.t_op1_94))
  (ia86.single_tag_test (reg rs) $tag.bytevector-tag)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr/peep (or (ia86.t_op1_95* rs rd)		; vector-like?
                             (ia86.t_op1_95))
  (ia86.single_tag_test (reg rs) $tag.vector-tag)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr (ia86.t_op2_96 regno)		; bytevector-like-ref
  (ia86.indexed_structure_ref regno $tag.bytevector-tag  $ex.bvlref #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.t_op3_97 regno y)		; bytevector-like-set!
  (ia86.indexed_structure_set_byte regno y $tag.bytevector-tag  0  $ex.bvlset))

(define-sassy-instr (ia86.t_op1_bvllength:bvl)     ; bytevector-like-length:bvl
  (ia86.indexed_structure_length_trusted $tag.bytevector-tag #t))

                                                  ; bytevector-like-ref:trusted
(define-sassy-instr (ia86.t_op2_bvlref:trusted rs2)
  (ia86.load_from_indexed_structure rs2 $tag.bytevector-tag #t)
  `(shl ,$r.result 2))

                                                 ; bytevector-like-set!:trusted
(define-sassy-instr (ia86.t_op3_bvlset:trusted regno y)
  (ia86.indexed_structure_set_byte_trusted regno y $tag.bytevector-tag))

(define-sassy-instr (ia86.t_op2_98 regno)		; sys$bvlcmp
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.bvlcmp 'bvlcmp))

(define-sassy-instr (ia86.t_op2_99 regno)		; vector-like-ref
  (ia86.indexed_structure_ref regno $tag.vector-tag  $ex.vlref  #f))

(define-sassy-instr (ia86.t_op3_100 regno y)		; vector-like-set!
  (ia86.indexed_structure_set_word regno y  $tag.vector-tag  0  $ex.vlset))

(define-sassy-instr (ia86.t_op1_101)		; vector-like-length
  (ia86.indexed_structure_length $tag.vector-tag $ex.vllen #f))

(define-sassy-instr (ia86.t_op1_102)		; bytevector-like-length
  (ia86.indexed_structure_length $tag.bytevector-tag $ex.bvllen #t))

(define-sassy-instr (ia86.t_op2_103 regno)		; remainder
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.remainder 'remainder))

(define-sassy-instr (ia86.t_op1_104)		; petit-patch-boot-code
  (ia86.mcall	$m.petit-patch-boot-code 'petit-patch-boot-code))

(define-sassy-instr (ia86.t_op1_105)		; syscall
  (ia86.mcall	$m.syscall 'syscall))

(define-sassy-instr (ia86.t_op1_106)		; creg
  (ia86.mcall	$m.creg 'creg))

(define-sassy-instr (ia86.t_op1_107)		; creg-set!
  (ia86.mcall	$m.creg-set! 'creg-set!))

(define-sassy-instr (ia86.t_op1_108)		; gc-counter
  `(mov	,$r.result (& ,$r.globals ,$g.gccnt)))

(define-sassy-instr (ia86.t_op1_109)		; major-gc-counter
  `(mov	,$r.result (& ,$r.globals ,$g.majorgccnt)))

(define-sassy-instr (ia86.t_op1_110)            ; machine-address
  `(shr	,$r.result 4)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.t_op2imm_128 imm)		; typetag-set!
  (ia86.const2regf $r.second imm)
  (ia86.mcall	$m.typetag-set 'typetag-set))

(define-sassy-instr/peep (or (ia86.t_op2imm_129* rs1 rd imm)	; eq?
                             (ia86.t_op2imm_129 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'z))

(define-sassy-instr/peep (or (ia86.t_op2imm_130* rs1 rd imm)	; +
                             (ia86.t_op2imm_130 imm))
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,(try-low (reg rs1)) ,$tag.fixtagmask)
    `(jnz short ,l1)
    (cond ((not (equal? rs1 rd))
           `(mov       ,(reg rd) ,(reg rs1))))
    `(add	,(reg rd) ,imm)
    `(jno short ,l2)
    `(sub	,(reg rd) ,imm)
    `(label ,l1)
    (cond ((not (result-reg? rs1))
           `(mov       ,$r.result ,(reg rs1))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.add 'add)
    (cond ((not (result-reg? rd))
           `(mov       ,(reg rd) ,$r.result)))
    `(label ,l2)))


(define-sassy-instr/peep (or (ia86.t_op2imm_131* rs1 rd imm)	; -
                             (ia86.t_op2imm_131 imm))
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    `(test	,(try-low (reg rs1)) ,$tag.fixtagmask)
    `(jnz short ,l1)
    (cond ((not (equal? rs1 rd))
           `(mov ,(reg rd) ,(reg rs1))))
    `(sub	,(reg rd) ,imm)
    `(jno short ,l2)
    `(add	,(reg rd) ,imm)
    `(label ,l1)
    (cond ((not (result-reg? rs1))
           `(mov       ,$r.result ,(reg rs1))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.subtract 'subtract)
    (cond ((not (result-reg? rd))
           `(mov       ,(reg rd) ,$r.result)))
    `(label ,l2)))

(define-sassy-instr (ia86.t_op2imm_132 imm)		; <
  (ia86.generic_imm_compare imm 'l  $m.numlt))

(define-sassy-instr (ia86.t_op2imm_133 imm)		; <=
  (ia86.generic_imm_compare imm 'le  $m.numle))

(define-sassy-instr (ia86.t_op2imm_134 imm)		; =
  (ia86.generic_imm_compare imm 'e  $m.numeq))

(define-sassy-instr (ia86.t_op2imm_135 imm)		; >
  (ia86.generic_imm_compare imm 'g  $m.numgt))

(define-sassy-instr (ia86.t_op2imm_136 imm)		; >=
  (ia86.generic_imm_compare imm 'ge  $m.numge))

;; XXX timer? XXX
(define-sassy-instr (ia86.t_reg_op2imm_branchf_132 rs imm l a-skip?)	; <
  (ia86.generic_imm_compare_branchf rs imm 'jnl $m.numlt l))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_133 rs imm l a-skip?)	; <=
  (ia86.generic_imm_compare_branchf rs imm 'jg  $m.numle l))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_134 rs imm l a-skip?)	; =
  (ia86.generic_imm_compare_branchf rs imm 'jne $m.numeq l))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_135 rs imm l a-skip?)	; >
  (ia86.generic_imm_compare_branchf rs imm 'jng $m.numgt l))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_136 rs imm l a-skip?)	; >=
  (ia86.generic_imm_compare_branchf rs imm 'jl  $m.numge l))


(define-sassy-instr (ia86.t_op2imm_137 imm)		; char<?
  (ia86.generic_char_imm_compare imm 'l  $ex.char<?))

(define-sassy-instr (ia86.t_op2imm_138 imm)		; char<=?
  (ia86.generic_char_imm_compare imm 'le  $ex.char<=?))

(define-sassy-instr (ia86.t_op2imm_139 imm)		; char=?
  (ia86.generic_char_imm_compare imm 'e  $ex.char=?))

(define-sassy-instr (ia86.t_op2imm_140 imm)		; char>?
  (ia86.generic_char_imm_compare imm 'g  $ex.char>?))

(define-sassy-instr (ia86.t_op2imm_141 imm)		; char>=?
  (ia86.generic_char_imm_compare imm 'ge  $ex.char>=?))

;;; The following five are probably a waste of effort.

(define-sassy-instr (ia86.t_op2imm_142 imm)		; string-ref
  (ia86.indexed_structure_ref_imm/hdr imm $tag.bytevector-tag  $hdr.string  $ex.sref  #t)
  `(shl	,$r.result ,$bitwidth.char-shift)
  `(or	,$r.result.low ,$imm.character))

(define-sassy-instr (ia86.t_op2imm_ustring-ref imm)     ; ustring-ref
  (ia86.indexed_structure_ref_imm/hdr imm $tag.bytevector-tag  
                                      (+ $imm.bytevector-header $tag.ustring-typetag)
                                      $ex.sref
                                      #f))

(define-sassy-instr (ia86.t_op2imm_143 imm)		; vector-ref
  (ia86.indexed_structure_ref_imm/hdr imm $tag.vector-tag  $hdr.vector  $ex.vref  #f))

(define-sassy-instr (ia86.t_op2imm_144 regno)		; bytevector-ref
  (ia86.indexed_structure_ref_imm/hdr regno $tag.bytevector-tag  $hdr.bytevector  $ex.bvref  #t)
  `(shl	,$r.result 2))
	
(define-sassy-instr (ia86.t_op2imm_145 imm)		; bytevector-like-ref
  (ia86.indexed_structure_ref_imm imm $tag.bytevector-tag  $ex.bvlref  #t)
  `(shl	,$r.result 2))

(define-sassy-instr (ia86.t_op2imm_146 imm)		; vector-like-ref
  (ia86.indexed_structure_ref_imm imm $tag.vector-tag  $ex.vlref  #f))

(define-sassy-instr (ia86.t_op1_200)		; most-positive-fixnum
  (ia86.const2regf $r.result #x7FFFFFFC))

(define-sassy-instr (ia86.t_op1_201)		; most-negative-fixnum
  (ia86.const2regf $r.result #x80000000))

(define-sassy-instr (ia86.t_op2_202 regno)		; fx+
  (ia86.fixnum_arithmetic regno 'add  'sub  $ex.fx+))

(define-sassy-instr (ia86.t_op2_203 regno)		; fx-
  (ia86.fixnum_arithmetic regno 'sub  'add  $ex.fx-))

(define-sassy-instr (ia86.t_op1_204)		; fx--
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           `(test	,$r.result.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           `(neg	,$r.result)
           `(jno short ,l2)
           ;; No need to undo: RESULT is unchanged
           `(label ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fx-- 'short l0)
           `(label ,l2)))
        (else
         `(neg	,$r.result))))

;; OPTIMIZEME: Introduce OP2IMM form of fx* that uses LEA tricks
(define-sassy-instr (ia86.t_op2_205 regno)              ; fx*
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.loadr	$r.temp regno)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           (ia86.loadr	$r.temp regno)
           `(sar	,$r.temp 2)
           `(imul	,$r.temp ,$r.result)
           `(jno short ,l2)
           `(label ,l1)
           (ia86.loadr	$r.temp regno)
           (ia86.exception_continuable $ex.fx* 'short l0)
           `(label ,l2)
           `(mov	,$r.result ,$r.temp)))
        ((is_hwreg regno)
         `(sar	,$r.result 2)
         `(imul	,$r.result ,(reg regno)))
        (else
         `(sar	,$r.result 2)
         `(imul	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_206 regno)		; fx=
  (ia86.fixnum_compare regno 'e  $ex.fx=))

(define-sassy-instr (ia86.t_op2_207 regno)		; fx<
  (ia86.fixnum_compare regno 'l  $ex.fx<))

(define-sassy-instr (ia86.t_op2_208 regno)		; fx<=
  (ia86.fixnum_compare regno 'le  $ex.fx<=))

(define-sassy-instr (ia86.t_op2_209 regno)		; fx>
  (ia86.fixnum_compare regno 'g  $ex.fx>))

(define-sassy-instr (ia86.t_op2_210 regno)		; fx>=
  (ia86.fixnum_compare regno 'ge  $ex.fx>=))

; Changed t_op2_2{11,12,13} to OP1.
; Do we refer to these as OP2 anywhere?
(define-sassy-instr (ia86.t_op1_211)		; fxzero?
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(test	,$r.result.low ,$tag.fixtagmask)
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxzero? 'short l0)
           `(label ,l1))))
  `(test	,$r.result ,$r.result)
  (ia86.setcc	$r.result 'z))

(define-sassy-instr (ia86.t_op1_212)		; fxpositive?
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(test	,$r.result.low ,$tag.fixtagmask)
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxpositive? 'short l0)
           `(label ,l1))))
  `(cmp	,$r.result 0)
  (ia86.setcc	$r.result 'g))

(define-sassy-instr (ia86.t_op1_213)		; fxnegative?
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label)))
           `(label ,l0)
           `(test	,$r.result.low ,$tag.fixtagmask)
           `(jz short ,l1)
           `(xor ,$r.second ,$r.second)
           (ia86.exception_continuable $ex.fxnegative? 'short l0)
           `(label ,l1))))
  `(cmp	,$r.result 0)
  (ia86.setcc $r.result 'l))

;;; The previous version of this code generator left an invalid pointer
;;; in $r.temp (aka $r.second, TEMP, SECOND, eax) when the operand
;;; is not a fixnum.  See ticket #686.
;;;
;;; FIXME: not tested because this appears to be semi-dead code.
;;; See src/Compiler/common.imp.sch.

(define-sassy-instr (ia86.fixnum_imm_arithmetic imm y z ex)
  (assert (fixnum? imm))
  (cond ((not (unsafe-code))
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(label ,l0)
           (ia86.const2regf $r.temp imm)
           `(test          ,$r.result.low ,$tag.fixtagmask)
           `(jnz short ,l1)
           `(,y	,$r.result ,$r.temp)
           `(jno short ,l2)
           `(,z	,$r.result ,$r.temp)
           `(label ,l1)
           (ia86.exception_continuable ex 'short l0)	; second is tmp so 2nd arg is in place
           `(label ,l2)))
        (else
         (ia86.const2regf $r.temp imm)
         `(,y	,$r.result ,$r.temp))))
	
(define-sassy-instr (ia86.t_op2imm_250 imm)           ; fx+
  (ia86.fixnum_imm_arithmetic imm 'add  'sub  $ex.fx+))

(define-sassy-instr (ia86.t_op2imm_251 imm)           ; fx-
  (ia86.fixnum_imm_arithmetic imm 'sub  'add  $ex.fx-))

;;; fixnum_imm_compare const, cc, ex
(define-sassy-instr (ia86.fixnum_imm_compare imm y z)
  (let ((l0 (fresh-label))
        (l1 (fresh-label)))
    (cond ((not (unsafe-code))
           `(label ,l0)
           (ia86.const2regf $r.temp imm)
           `(or	,$r.temp ,$r.result)
           `(test	,$r.temp.low ,$tag.fixtagmask)
           `(jz short ,l1)
           (ia86.const2regf $r.temp imm)
           (ia86.exception_continuable z 'short l0)	; second is tmp so 2nd arg is in place
           `(label ,l1)))
    (ia86.const2regf $r.temp imm)
    `(cmp	,$r.result 	,$r.temp)
    (ia86.setcc $r.result y)))

(define-sassy-instr (ia86.t_op2imm_253 imm)		; fx=
  (ia86.fixnum_imm_compare imm 'e  $ex.fx=))

(define-sassy-instr (ia86.t_op2imm_254 imm)		; fx<
  (ia86.fixnum_imm_compare imm 'l  $ex.fx<))

(define-sassy-instr (ia86.t_op2imm_255 imm)		; fx<=
  (ia86.fixnum_imm_compare  imm 'le  $ex.fx<=))

(define-sassy-instr (ia86.t_op2imm_256 imm)		; fx>
  (ia86.fixnum_imm_compare  imm 'g  $ex.fx>))

(define-sassy-instr (ia86.t_op2imm_257 imm)		; fx>=
  (ia86.fixnum_imm_compare  imm 'ge  $ex.fx>=))

;;; Unsafe/trusted primitives

(define-sassy-instr/peep (or (ia86.t_op1_401* rs rd)		; vector-length:vec
                             (ia86.t_op1_401))
  `(mov	,(reg rd) (& ,(reg rs) ,(- $tag.vector-tag)))
  `(shr	,(reg rd) 8))

(define-sassy-instr/peep (or (ia86.t_op2_402* rs1 rd rs2)	; vector-ref:trusted
                             (ia86.t_op2_402 rs2))
  (cond ((is_hwreg rs2)
         `(mov	,(reg rd) (& ,(reg rs1) 
                             ,(reg rs2)
                             ,(- $bytewidth.wordsize $tag.vector-tag))))
        (else
         (ia86.loadr	$r.temp rs2)
         `(mov	,(reg rd) (& ,(reg rs1) 
                             ,$r.temp 
                             ,(- $bytewidth.wordsize $tag.vector-tag))))))

(define-sassy-instr (ia86.t_op3_403 regno y)		; vector-set!:trusted
  (ia86.do_indexed_structure_set_word $r.result regno y $tag.vector-tag))

(define-sassy-instr (ia86.t_op3_403:nwb regno y)	; vector-set!:trusted:nwb
  (ia86.do_indexed_structure_set_word:nwb $r.result regno y $tag.vector-tag))

(define-sassy-instr (ia86.t_reg_op3_403 hwregno regno2 regno3)
  (ia86.do_indexed_structure_set_word hwregno regno2 regno3 $tag.vector-tag))

(define-sassy-instr (ia86.t_reg_op3_403:nwb hwregno regno2 regno3)
  (ia86.do_indexed_structure_set_word:nwb hwregno regno2 regno3 $tag.vector-tag))

(define-sassy-instr/peep (or (ia86.t_op1_404* rs rd)		; car:pair
                             (ia86.t_op1_404))
  `(mov	,(reg rd) (& ,(reg rs) ,(- $tag.pair-tag))))

(define-sassy-instr/peep (or (ia86.t_op1_405* rs rd)		; cdr:pair
                             (ia86.t_op1_405))
  `(mov	,(reg rd) (& ,(reg rs) ,(+ (- $tag.pair-tag) $bytewidth.wordsize))))

(define-sassy-instr/peep (or (ia86.t_op2_406* rs1 rd rs2)		; =:fix:fix
                             (ia86.t_op2_406 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'e))

(define-sassy-instr/peep (or (ia86.t_op2_407* rs1 rd rs2)		; <:fix:fix
                             (ia86.t_op2_407 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'l))

(define-sassy-instr/peep (or (ia86.t_op2_408* rs1 rd rs2)		; <=:fix:fix
                             (ia86.t_op2_408 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'le))

(define-sassy-instr/peep (or (ia86.t_op2_409* rs1 rd rs2)		; >=:fix:fix
                             (ia86.t_op2_409 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'ge))

(define-sassy-instr/peep (or (ia86.t_op2_410* rs1 rd rs2)		; >:fix:fix
                             (ia86.t_op2_410 rs2))
  (ia86.trusted_fixnum_compare rs1 rd rs2 'g))

(define-sassy-instr (ia86.trusted_fixnum_compare_branch hwregno regno l jnc)
  (cond ((is_hwreg regno)
         `(cmp	,(reg hwregno) ,(reg regno)))
        (else
         `(cmp	,(reg hwregno) (& ,$r.globals ,(g-reg regno)))))
  `(,jnc try-short ,l))

;; For these, we jump on the *opposite* of the condition we are
;; testing for, b/c the check jumps if condition *fails*
(define-sassy-instr (ia86.t_reg_op2_check_406 hwregno regno2 l)	; =:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jne))

(define-sassy-instr (ia86.t_reg_op2_check_407 hwregno regno2 l)	; <:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jge))

(define-sassy-instr (ia86.t_reg_op2_check_408 hwregno regno2 l)	; <=:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jg))

(define-sassy-instr (ia86.t_reg_op2_check_409 hwregno regno2 l)	; >=:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jl))

(define-sassy-instr (ia86.t_reg_op2_check_410 hwregno regno2 l)	; >:fix:fix
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jle))


(define-sassy-instr (ia86.t_reg_op2_branchf_406 hwregno regno2 l a-skip?)	; =:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jne))

(define-sassy-instr (ia86.t_reg_op2_branchf_407 hwregno regno2 l a-skip?)	; <:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jge))

(define-sassy-instr (ia86.t_reg_op2_branchf_408 hwregno regno2 l a-skip?)	; <=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jg))

(define-sassy-instr (ia86.t_reg_op2_branchf_409 hwregno regno2 l a-skip?)	; >=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jl))

(define-sassy-instr (ia86.t_reg_op2_branchf_410 hwregno regno2 l a-skip?)	; >:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_branch hwregno regno2 l 'jle))

(define-sassy-instr/peep (or (ia86.t_op2imm_450* rs1 rd imm)	; vector-ref:trusted
                             (ia86.t_op2imm_450 imm))
  `(mov	,(reg rd) (& ,(reg rs1) ,imm ,(- $bytewidth.wordsize $tag.vector-tag))))

                                                  ; bytevector-like-ref:trusted
(define-sassy-instr (ia86.t_op2imm_bvlref:trusted idx)
  (ia86.load_from_indexed_structure_imm idx $tag.bytevector-tag #t)
  `(shl ,$r.result 2))

(define-sassy-instr/peep (or (ia86.t_op2imm_451* rs1 rd imm)		; =:fix:fix
                             (ia86.t_op2imm_451 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'e))

(define-sassy-instr/peep (or (ia86.t_op2imm_452* rs1 rd imm)		; <:fix:fix
                             (ia86.t_op2imm_452 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'l))

(define-sassy-instr/peep (or (ia86.t_op2imm_453* rs1 rd imm)		; <=:fix:fix
                             (ia86.t_op2imm_453 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'le))

(define-sassy-instr/peep (or (ia86.t_op2imm_454* rs1 rd imm)		; >:fix:fix
                             (ia86.t_op2imm_454 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'g))

(define-sassy-instr/peep (or (ia86.t_op2imm_455* rs1 rd imm)		; >=:fix:fix
                             (ia86.t_op2imm_455 imm))
  `(cmp	,(reg rs1) ,imm)
  (ia86.setcc	(reg rd) 'ge))

(define-sassy-instr (ia86.trusted_fixnum_compare_imm_branch hwregno imm l jnc)
  `(cmp	,(reg hwregno) ,imm)
  `(,jnc try-short ,l))

(define-sassy-instr (ia86.t_reg_op2imm_check_451 hwregno imm l)	; =:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jne))

(define-sassy-instr (ia86.t_reg_op2imm_check_452 hwregno imm l)	; <:fix:fix
 (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jnl))

(define-sassy-instr (ia86.t_reg_op2imm_check_453 hwregno imm l) ; <=:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jg))

(define-sassy-instr (ia86.t_reg_op2imm_check_454 hwregno imm l)	; >:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jng))

(define-sassy-instr (ia86.t_reg_op2imm_check_455 hwregno imm l)	; >=:fix:fix
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jl))


(define-sassy-instr (ia86.t_reg_op2imm_branchf_451 hwregno imm l a-skip?) ; =:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jne))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_452 hwregno imm l a-skip?) ; <:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jnl))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_453 hwregno imm l a-skip?) ; <=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jg))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_454 hwregno imm l a-skip?) ; >:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jng))

(define-sassy-instr (ia86.t_reg_op2imm_branchf_455 hwregno imm l a-skip?) ; >=:fix:fix
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.trusted_fixnum_compare_imm_branch hwregno imm l 'jl))


;;; Introduced by representation inference.  Trusted.
;;;
;;; FIXME: there should be peepholes for these as well

(define-sassy-instr (ia86.t_op2_500 regno)		; +:idx:idx
  (cond ((is_hwreg regno)
         `(add	,$r.result ,(reg regno)))
        (else
         `(add	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_501 regno)		; +:fix:fix
  (let ((l1 (fresh-label)))
    (cond ((and (is_hwreg regno)
                (not (result-reg? regno)))
           `(add	,$r.result ,(reg regno))
           `(jno short ,l1)
           (ia86.loadr	$r.temp regno)
           `(sub	,$r.result ,$r.temp)
           ; second is temp so 2nd arg is now in place
           (ia86.mcall	$m.add 'add))
          (else
           (ia86.loadr	$r.temp regno)
           `(add	,$r.result ,$r.temp)
           `(jno short ,l1)
           `(sub	,$r.result ,$r.temp)
           ; second is temp so 2nd arg is already in place
           (ia86.mcall	$m.add 'add)))
    `(label ,l1)))

(define-sassy-instr (ia86.t_op2_502 regno)		; -:idx:idx
  (cond ((is_hwreg regno)
         `(sub	,$r.result ,(reg regno)))
        (else
         `(sub	,$r.result (& ,$r.globals ,(g-reg regno))))))

(define-sassy-instr (ia86.t_op2_503 regno)		; -:fix:fix
  (let ((l1 (fresh-label)))
    (cond ((and (is_hwreg regno)
                (not (result-reg? regno)))
           `(sub	,$r.result ,(reg regno))
           `(jno short ,l1)
           (ia86.loadr	$r.temp regno)
           `(add	,$r.result ,$r.temp)
           ; second is temp so 2nd arg is now in place
           (ia86.mcall	$m.subtract 'subtract))
          (else
           (ia86.loadr	$r.temp regno)
           `(sub	,$r.result ,$r.temp)
           `(jno short ,l1)
           `(add	,$r.result ,$r.temp)
           ; second is temp so 2nd arg is already in place
           (ia86.mcall	$m.subtract 'subtract)))
    `(label ,l1)))

(define-sassy-instr/peep (or (ia86.t_op2imm_520* rs1 rd imm)	; +:idx:idx
                             (ia86.t_op2imm_520 imm))
  `(lea ,(reg rd) (& ,(reg rs1) ,imm)))

(define-sassy-instr/peep (or (ia86.t_op2imm_521* rs1 rd imm)	; +:fix:fix
                             (ia86.t_op2imm_521 imm))
  (cond ((not (equal? rs1 rd))
         `(mov ,(reg rd) ,(reg rs1))))
  (let ((l1 (fresh-label)))
    `(add	,(reg rd) ,imm)
    `(jno short ,l1)
    `(sub	,(reg rd) ,imm)
    (cond ((not (result-reg? rd))
           `(mov       ,$r.result ,(reg rd))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.add 'add)
    (cond ((not (result-reg? rd))
           `(mov    ,(reg rd) ,$r.result)))
    `(label ,l1)))

(define-sassy-instr/peep (or (ia86.t_op2imm_522* rs1 rd imm)	; -:idx:idx
                             (ia86.t_op2imm_522 imm))
  `(lea ,(reg rd) (& ,(reg rs1) ,(- imm))))

(define-sassy-instr/peep (or (ia86.t_op2imm_523* rs1 rd imm)	; -:fix:fix
                             (ia86.t_op2imm_523 imm))
  (cond ((not (equal? rs1 rd))
         `(mov  ,(reg rd) ,(reg rs1))))
  (let ((l1 (fresh-label)))
    `(sub	,(reg rd) ,imm)
    `(jno short ,l1)
    `(add	,(reg rd) ,imm)
    (cond ((not (result-reg? rd))
           `(mov      ,$r.result ,(reg rd))))
    `(mov	,$r.second ,imm)
    (ia86.mcall	$m.subtract 'subtract)
    (cond ((not (result-reg? rd))
           `(mov     ,(reg rd) ,$r.result)))
    `(label ,l1)))

; Trusted flonum operations.

(define-sassy-instr (ia86.t_op2_add_floflo regno)	; +:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.fladd 'fladd))
	
(define-sassy-instr (ia86.t_op2_sub_floflo regno)	; -:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.flsub 'flsub))

(define-sassy-instr (ia86.t_op2_mul_floflo regno)	; *:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.flmul 'flmul))
	
(define-sassy-instr (ia86.t_op2_div_floflo regno)	; /:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.fldiv 'fldiv))
	
(define-sassy-instr (ia86.t_op2_lt_floflo regno)	; <:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.fllt 'fllt))

(define-sassy-instr (ia86.t_op2_lteq_floflo regno)	; <+:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.flleq 'flleq))

(define-sassy-instr (ia86.t_op2_gt_floflo regno)	; >:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.flgt 'flgt))

(define-sassy-instr (ia86.t_op2_gteq_floflo regno)	; >=:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.flgeq 'flgeq))

(define-sassy-instr (ia86.t_op2_eq_floflo regno)	; =:flo:flo
  (ia86.loadr	$r.second regno)
  (ia86.mcall	$m.fleq 'fleq))

; End of flonum operations.

(define-sassy-instr (ia86.reg_generic_compare_lowimm_branchf imm rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(try-low (reg rs)) ,imm)
  `(jne try-short ,l))

(define-sassy-instr (ia86.reg_generic_compare_imm_branchf imm rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  `(cmp	,(reg rs) ,imm)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_branchf_null? rs l a-skip?)
  (ia86.reg_generic_compare_lowimm_branchf $imm.null rs l a-skip?))

(define-sassy-instr (ia86.t_reg_op1_branchf_eof_object? rs l a-skip?)
  (ia86.reg_generic_compare_imm_branchf $imm.eof rs l a-skip?))

(define-sassy-instr (ia86.t_reg_op1_branchf_pair? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.single_tag_test (reg rs) $tag.pair-tag)
  `(jnz try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_branchf_zero? rs l a-skip?)
  (let ((l1 (fresh-label))
        (l2 (fresh-label)))
    (cond ((not a-skip?)
           (ia86.timer_check)))
    (ia86.fixnum_test_temp_is_free rs)
    `(jz short ,l1)
    `(mov ,$r.result ,(reg rs))
    (ia86.mcall	$m.zerop 'zerop)
    `(cmp ,$r.result.low ,$imm.false)
    `(je try-short ,l)
    `(jmp short ,l2)
    `(label ,l1)
    `(cmp       ,(reg rs) 0)
    `(jnz try-short ,l)
    `(label ,l2)))

(define-sassy-instr (ia86.t_reg_op1_branchf_fixnum? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (cond ((hwreg_has_low rs)
         `(test ,(reg-low rs) ,$tag.fixtagmask))
        (else
         `(mov ,$r.result ,(reg rs))
         `(test ,$r.result.low ,$tag.fixtagmask)))
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_fixnum? rs l)
  (cond ((hwreg_has_low rs)
         `(test ,(reg-low rs) ,$tag.fixtagmask))
        (else
         `(mov ,$r.result ,(reg rs))
         `(test ,$r.result.low ,$tag.fixtagmask)))
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_branchf_flonum? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.flonum)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_flonum? rs l)
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.flonum)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_pair? rs l)
  (ia86.single_tag_test (reg rs) $tag.pair-tag)
  `(jne try-short ,l))

; FIXME: the op1/branchf code duplicates the op1/check code

(define-sassy-instr (ia86.t_reg_op1_branchf_vector? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.double_tag_test (reg rs) $tag.vector-tag $hdr.vector)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_vector? rs l)
  (ia86.double_tag_test (reg rs) $tag.vector-tag $hdr.vector)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_branchf_bytevector? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.bytevector)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_bytevector? rs l)
  (ia86.double_tag_test (reg rs) $tag.bytevector-tag $hdr.bytevector)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_branchf_structure? rs l a-skip?)
  (cond ((not a-skip?)
         (ia86.timer_check)))
  (ia86.double_tag_test (reg rs) $tag.vector-tag $hdr.struct)
  `(jne try-short ,l))

(define-sassy-instr (ia86.t_reg_op1_check_structure? rs l)
  (ia86.double_tag_test (reg rs) $tag.vector-tag $hdr.struct)
  `(jne try-short ,l))

;; "Reflective ops"; processor level performance measurement.
;; Very sketch!

(define-sassy-instr (ia86.t_op1_1000)           ; larceny-timestamp!
  (let ((l3 (fresh-label)))
    (ia86.double_tag_test $r.result $tag.bytevector-tag $hdr.bytevector)
    `(jnz short ,l3)                        ;; leave RESULT alone if arg bad
    (cond ((not (unsafe-code))
           `(mov ,$r.temp (& ,$r.result ,(- $tag.bytevector-tag)))
           `(shr ,$r.temp 8)
           `(cmp ,$r.temp 8) ;; (at least 2 words)
           `(jl short ,l3)))                    ;; leave RESULT alone if arg bad
    `(mov (& ,$r.globals ,$g.regalias-ebx) ebx) ;; save RESULT
    `(mov (& ,$r.globals ,$g.regalias-ecx) ecx) ;;  and
    `(mov (& ,$r.globals ,$g.regalias-edx) edx) ;;   others
    ;; `(cpuid)                                ;; serialize (writes eax, ebx, ecx, edx)
    `(rdtsc)                                ;; writes 64 bit value into edx:eax
    `(mov ebx (& ,$r.globals ,$g.regalias-ebx)) ;; restore RESULT
    `(mov (& ,$r.result ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes)) 
          eax)                              ;; save timestamp low bits
    `(mov (& ,$r.result ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes $bytewidth.wordsize))
          edx)                              ;; save timestamp high bits
    ;; `(cpuid)                                ;; serialize (writes eax, ebx, ecx, edx)
    `(mov ebx (& ,$r.globals ,$g.regalias-ebx)) ;; restore RESULT
    `(mov ecx (& ,$r.globals ,$g.regalias-ecx)) ;;  and
    `(mov edx (& ,$r.globals ,$g.regalias-edx)) ;;   others
    `(label ,l3)))

(define-sassy-instr (ia86.t_op1_1001)
  `(rdpmc))

;;; eof
