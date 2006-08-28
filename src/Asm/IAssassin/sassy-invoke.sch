;;; i386 macros for the invocation portsion of the MacScheme instruction set.
;;;
;;; $Id: i386-instr.asm 2715 2005-12-14 22:27:51Z tov $

;;; See notes at top of sassy-instr.sch, as well as the macro definitons
;;; there (though I have pasted them here so that it is safe to compile
;;; this file).

;;; (It is good to keep these macro definitions in sync with top of
;;; sassy-instr.sch)
(define-syntax define-sassy-instr
  (syntax-rules ()
    ((_ (NAME ARGS ...) BODY ...)
     (define (NAME ARGS ...) (seqlist BODY ...))))) ; do this for now...

;; Has to be a macro since order of eval is undef'd in Scheme
(define-syntax seqlist 
  (syntax-rules (begin cond let quasiquote)
    ((seqlist) 
     (list))
    ((_ (begin EXP ...) EXPS ...)
     (append (begin EXP ...) 
             (seqlist EXPS ...)))
    ((_ (cond (Q A ...) ...) EXPS ...)
     (append (mcond (Q (seqlist A ...)) ...) 
             (seqlist EXPS ...)))
    ((_ (let ((I E) ...) BODY ...) EXPS ...)
     (append (let ((I E) ...) (seqlist BODY ...)) 
             (seqlist EXPS ...)))
    ;; Note in below two clauses, first uses CONS, second uses APPEND
    ((_ (quasiquote EXP1) EXPS ...)
     (cons (quasiquote EXP1) (seqlist EXPS ...)))
    ((_ EXP1 EXPS ...)
     (let ((v EXP1))
       (append v (seqlist EXPS ...))))))

(define-syntax mcond
  (syntax-rules (else)
    ((_) '())
    ((_ (else A)) A)
    ((_ (Q A) C ...) (if Q A (mcond C ...)))))

;;; Note the millicode for M_INVOKE_EX is used to check for timer
;;; exception as well, and must check the timer first.

(define-sassy-instr (ia86.T_INVOKE n)
  (cond ((unsafe-code) ;; 32 bytes for n=0, 35 bytes o/w
         (ia86.timer_check)
         (ia86.storer 0 'RESULT)
         `(mov TEMP RESULT)
         (ia86.const2regf 'RESULT (fixnum n))
         `(mov TEMP (& TEMP ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
         `(add TEMP ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES))
	 `(jmp TEMP))
        (else ;; 37 bytes for n=0, 40 bytes o/w
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(dec (dword (& GLOBALS ,$g.timer)))
           `(jz short ,L0)
           `(label ,L1)
	   `(lea TEMP (& RESULT ,(- $tag.procedure-tag)))
	   `(test TEMP_LOW tag_mask)
	   `(jnz short ,L0)
           `(mov TEMP (& TEMP ,PROC_CODEVECTOR_NATIVE))
           (ia86.storer 0 'RESULT)
           (ia86.const2regf 'RESULT (fixnum n))
           `(add TEMP ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES))
           `(jmp TEMP)
           `(label ,L0)
	   (ia86.mcall $m.invoke-ex 'invoke-ex)
           `(jmp short ,L1)
           ))))

;;; Introduced by peephole optimization.
;;; 
;;; The trick here is that the tag check for procedure-ness will
;;; catch undefined variables too.  So there is no need to see
;;; if the global has an undefined value, specifically.  The
;;; exception handler figures out the rest.

(define-sassy-instr (ia86.T_GLOBAL_INVOKE g n)
  (cond ((unsafe-globals)
         (ia86.T_GLOBAL g)
         (ia86.T_INVOKE n))
        (else
         (let ((L1 (fresh-label))
               (L2 (fresh-label)))
           (ia86.loadc	'RESULT g)		; global cell
           `(label ,L2)
	   `(mov TEMP                    ;   dereference
                 (& RESULT ,(- $tag.pair-tag)))
	   `(inc TEMP)			; really TEMP += PROC_TAG-8
	   `(test TEMP_LOW tag_mask)	; tag test
	   `(jnz short ,L1)
	   `(dec (dword (& GLOBALS ,$g.timer))) ; timer
	   `(jz short ,L1)                ;   test
	   `(dec TEMP)                   ; undo ptr adjustment
	   (ia86.storer 0 'TEMP)              ; save proc ptr
	   (ia86.const2regf 'RESULT           ; argument count
                            (fixnum n))
	   `(mov TEMP	(& TEMP ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
           `(add TEMP ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES))
           `(jmp TEMP)
           `(label ,L1)
	   (ia86.mcall $m.global-invoke-ex 'global-invoke-ex) ; RESULT has global cell (always)
	   `(jmp short ,L2)		; Since TEMP is dead following timer interrupt
           ))))

(define-sassy-instr (ia86.T_SETRTN_INVOKE n)
  (let ((ign (if (not (memv n *did-emit-setrtn-invoke*))
                 (set! *did-emit-setrtn-invoke* 
                       (cons n *did-emit-setrtn-invoke*)))))
    (cond ((unsafe-code)
           (error 'T_SETRTN_INVOKE "Not implemented unsafely yet"))
          (else 
           ;; For SETRTN, see patch-code below
           ;; INVOKE
           (let ((L0 (fresh-label))
                 (L1 (fresh-label)))
             `(dec (dword (& GLOBALS ,$g.timer)))
             `(jnz short ,L1)
             `(label ,L0)
             (ia86.mcall $m.invoke-ex 'invoke-ex)
             `(label ,L1)
             `(lea TEMP (& RESULT ,(- $tag.procedure-tag)))
             `(test TEMP_LOW tag_mask)
             `(jnz short ,L0)
             `(mov TEMP (& TEMP ,PROC_CODEVECTOR_NATIVE))
             (ia86.storer 0 'RESULT)
             ;; n stored in RESULT via patch-code
             ;; aligning the code here allows us to eliminate 
             ;; the add&and from the patch code (saving 9 bytes).
             `(align code_align)
             ;;   3 bytes
             `(add TEMP ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES))
             ;; + 5 bytes = 8 bytes; retaddr is aligned!
             `(call ,(setrtn-invoke-patch-code-label n))
             )))))

(define-sassy-instr (ia86.T_SETRTN_BRANCH Ly)
  (let ((ign (if (not (member Ly *did-emit-setrtn-branch*))
                 (set! *did-emit-setrtn-branch* 
                       (cons Ly *did-emit-setrtn-branch*)))))
    (ia86.timer_check)
    `(align ,code_align -1)
    `(call ,(setrtn-branch-patch-code-label Ly))))

(define-sassy-instr (ia86.T_SETRTN_SKIP Ly)
  (let ((ign (if (not (member Ly *did-emit-setrtn-branch*))
                 (set! *did-emit-setrtn-branch* 
                       (cons Ly *did-emit-setrtn-branch*)))))
    `(align ,code_align -1)
    `(call ,(setrtn-branch-patch-code-label Ly))))

(define (setrtn-invoke-patch-code-label n)
  (string->symbol (string-append "setrtn-invoke-patch-code-label" 
                                 (number->string n))))

(define (setrtn-branch-patch-code-label l)
  (string->symbol (string-append "setrtn-branch-patch-code-label" 
                                 l)))

(define (emit-setrtn-invoke-patch-code as n)
  (define (emit x) (apply emit-sassy as x))
  (emit `(label ,(setrtn-invoke-patch-code-label n)))
  (emit `(pop (& CONT ,STK_RETADDR)))  ;; pre-aligned return address
  (for-each emit (ia86.const2regf 'RESULT (fixnum n)))
  (emit `(jmp TEMP)))
         
(define (emit-setrtn-branch-patch-code as l)
  (define (emit x) (apply emit-sassy as x))
  (emit `(label ,(setrtn-branch-patch-code-label l)))
  (emit `(pop (& CONT ,STK_RETADDR)))  ;; pre-aligned return address 
  (emit `(jmp ,(t_label l))))

(define-sassy-instr (ia86.T_APPLY x y)
  (ia86.timer_check)
  (ia86.loadr	'TEMP y)
  `(mov	(& GLOBALS ,$g.third) TEMP)
  (ia86.loadr	SECOND x)
  (ia86.mcall	$m.apply 'apply)
  (ia86.loadr	'TEMP 0)
  `(mov	TEMP (& TEMP ,(+ (- $tag.procedure-tag) PROC_CODEVECTOR_NATIVE)))
  `(add TEMP ,(+ (- $tag.bytevector-tag) BVEC_HEADER_BYTES))
  `(jmp	TEMP))

