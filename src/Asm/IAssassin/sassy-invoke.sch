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
  (syntax-rules (cond let quasiquote)
    ((seqlist) 
     (list))
    ((_ (cond (Q A ...) ...) EXPS ...)
     (append (mcond '() (Q (seqlist A ...)) ...) 
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
    ((_ end) end)
    ((_ end (else A)) A)
    ((_ end (Q A) C ...) (if Q A (mcond end C ...)))))

;;; Note the millicode for M_INVOKE_EX is used to check for timer
;;; exception as well, and must check the timer first.

(define-sassy-instr (ia86.T_INVOKE n)
  (cond ((unsafe-code) ;; 32 bytes for n=0, 35 bytes o/w
         (ia86.timer_check)
         (ia86.storer 0 $r.result) ;; OPTIMIZEME
         `(mov ,$r.temp ,$r.result)
         (ia86.const2regf $r.result (fixnum n))
         `(mov ,$r.temp (& ,$r.temp ,(+ (- $tag.procedure-tag) $proc.codevector)))
         `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
	 `(jmp ,$r.temp))
        (else ;; 37 bytes for n=0, 40 bytes o/w
         (let ((L0 (fresh-label))
               (L1 (fresh-label))
               (L2 (fresh-label)))
           `(dec (dword (& ,$r.globals ,$g.timer)))
           `(jz short ,L0)
           `(label ,L1)
	   `(lea ,$r.temp (& ,$r.result ,(- $tag.procedure-tag)))
	   `(test ,$r.temp.low ,$tag.tagmask)
	   `(jnz short ,L0)
           `(mov ,$r.temp (& ,$r.temp ,$proc.codevector))
           (ia86.storer 0 $r.result)
           (ia86.const2regf $r.result (fixnum n))
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           `(jmp ,$r.temp)
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
           (ia86.loadc	$r.result g)		; global cell
           `(label ,L2)
	   `(mov ,$r.temp                    ;   dereference
                 (& ,$r.result ,(- $tag.pair-tag)))
	   `(inc ,$r.temp)			; really ,TEMP += PROC_TAG-8
	   `(test ,$r.temp.low ,$tag.tagmask)	; tag test
	   `(jnz short ,L1)
	   `(dec (dword (& ,$r.globals ,$g.timer))) ; timer
	   `(jz short ,L1)                ;   test
	   `(dec ,$r.temp)                   ; undo ptr adjustment
	   (ia86.storer 0 $r.temp)              ; save proc ptr
	   (ia86.const2regf $r.result           ; argument count
                            (fixnum n))
	   `(mov ,$r.temp	(& ,$r.temp ,(+ (- $tag.procedure-tag) $proc.codevector)))
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           `(jmp ,$r.temp)
           `(label ,L1)
	   (ia86.mcall $m.global-invoke-ex 'global-invoke-ex) ; $r.result has global cell (always)
	   `(jmp short ,L2)		; Since ,TEMP is dead following timer interrupt
           ))))

(define-sassy-instr (ia86.T_SETRTN_INVOKE n)
  (let ((ign (if (not (memv n *did-emit-setrtn-invoke*))
                 (set! *did-emit-setrtn-invoke* 
                       (cons n *did-emit-setrtn-invoke*)))))
    (cond ((unsafe-code) ;; (see notes in unsafe version)
           (ia86.timer_check)
           (ia86.storer 0 $r.result)
           `(mov ,$r.temp (& ,$r.result ,(+ (- $tag.procedure-tag) $proc.codevector)))
           `(align ,code_align)
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           `(call ,(setrtn-invoke-patch-code-label n)))
          (else 
           ;; For SETRTN, see patch-code below
           ;; INVOKE
           (let ((L0 (fresh-label))
                 (L1 (fresh-label)))
             `(dec (dword (& ,$r.globals ,$g.timer)))
             `(jnz short ,L1)
             `(label ,L0)
             (ia86.mcall $m.invoke-ex 'invoke-ex)
             `(label ,L1)
             `(lea ,$r.temp (& ,$r.result ,(- $tag.procedure-tag)))
             `(test ,$r.temp.low ,$tag.tagmask)
             `(jnz short ,L0)
             `(mov ,$r.temp (& ,$r.temp ,$proc.codevector))
             (ia86.storer 0 $r.result)
             ;; n stored in RESULT via patch-code
             ;; aligning the code here allows us to eliminate 
             ;; the add&and from the patch code (saving 9 bytes).
             `(align ,code_align)
             ;;   3 bytes
             `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
             ;; + 5 bytes = 8 bytes; retaddr is aligned!
             `(call ,(setrtn-invoke-patch-code-label n))
             )))))

(define-sassy-instr (ia86.T_SETRTN_BRANCH Ly)
  (let ((ign (if (not (member Ly *did-emit-setrtn-branch*))
                 (set! *did-emit-setrtn-branch* 
                       (cons Ly *did-emit-setrtn-branch*)))))
    `(align ,code_align -1)
    `(call ,(setrtn-branch-patch-code-label Ly))))

(define-sassy-instr (ia86.T_SETRTN_SKIP Ly) ;; FIXME: shouldn't decrement timer.
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
                                 (symbol->string l))))

(define (emit-setrtn-invoke-patch-code as n)
  (define (emit x) (apply emit-sassy as x))
  (emit `(label ,(setrtn-invoke-patch-code-label n)))
  (emit `(pop (& ,$r.cont ,$stk.retaddr)))  ;; pre-aligned return address
  (for-each emit (do-sassy-instr ia86.const2regf $r.result (fixnum n)))
  (emit `(jmp ,$r.temp)))
         
(define (emit-setrtn-branch-patch-code as l)
  (define (emit x) (apply emit-sassy as x))
  (emit `(label ,(setrtn-branch-patch-code-label (t_label l))))
  (emit `(pop (& ,$r.cont ,$stk.retaddr)))  ;; pre-aligned return address 
  (emit `(dec (dword (& ,$r.globals ,$g.timer))))
  (emit `(jnz ,(t_label l)))
  (emit `(call (& ,$r.globals ,$m.timer-exception)))
  (emit `(align ,code_align))
  (emit `(jmp ,(t_label l))))

(define-sassy-instr (ia86.T_APPLY x y)
  (ia86.timer_check)
  (ia86.loadr	$r.temp y)
  `(mov	(& ,$r.globals ,$g.third) ,$r.temp)
  (ia86.loadr	$r.second x)
  (ia86.mcall	$m.apply 'apply)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.codevector)))
  `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
  `(jmp	,$r.temp))

