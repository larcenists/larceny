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

(define-sassy-instr (ia86.t_invoke n)
  (cond ((unsafe-code) ;; 32 bytes for n=0, 35 bytes o/w
         (ia86.timer_check)
         (ia86.storer 0 $r.result) ;; OPTIMIZEME
         `(mov ,$r.temp ,$r.result)
         (ia86.const2regf $r.result (fixnum n))
         `(mov ,$r.temp (& ,$r.temp ,(+ (- $tag.procedure-tag) $proc.codevector)))
         `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
	 `(jmp ,$r.temp))
        (else ;; 37 bytes for n=0, 40 bytes o/w
         (let ((l0 (fresh-label))
               (l1 (fresh-label))
               (l2 (fresh-label)))
           `(dec (dword (& ,$r.globals ,$g.timer)))
           `(jz short ,l0)
           `(label ,l1)
	   `(lea ,$r.temp (& ,$r.result ,(- $tag.procedure-tag)))
	   `(test ,$r.temp.low ,$tag.tagmask)
	   `(jnz short ,l0)
           `(mov ,$r.temp (& ,$r.temp ,$proc.codevector))
           (ia86.storer 0 $r.result)
           (ia86.const2regf $r.result (fixnum n))
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           `(jmp ,$r.temp)
           `(label ,l0)
	   (ia86.mcall $m.invoke-ex 'invoke-ex)
           `(jmp short ,l1)
           ))))

;;; Introduced by peephole optimization.
;;; 
;;; The trick here is that the tag check for procedure-ness will
;;; catch undefined variables too.  So there is no need to see
;;; if the global has an undefined value, specifically.  The
;;; exception handler figures out the rest.

(define-sassy-instr (ia86.t_global_invoke g n)
  (cond ((unsafe-globals)
         (ia86.t_global g)
         (ia86.t_invoke n))
        (else
         (let ((l1 (fresh-label))
               (l2 (fresh-label)))
           (ia86.loadc	$r.result g)		; global cell
           `(label ,l2)
	   `(mov ,$r.temp                    ;   dereference
                 (& ,$r.result ,(- $tag.pair-tag)))
	   `(inc ,$r.temp)			; really ,TEMP += PROC_TAG-8
	   `(test ,$r.temp.low ,$tag.tagmask)	; tag test
	   `(jnz short ,l1)
	   `(dec (dword (& ,$r.globals ,$g.timer))) ; timer
	   `(jz short ,l1)                ;   test
	   `(dec ,$r.temp)                   ; undo ptr adjustment
	   (ia86.storer 0 $r.temp)              ; save proc ptr
	   (ia86.const2regf $r.result           ; argument count
                            (fixnum n))
	   `(mov ,$r.temp	(& ,$r.temp ,(+ (- $tag.procedure-tag) $proc.codevector)))
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           `(jmp ,$r.temp)
           `(label ,l1)
	   (ia86.mcall $m.global-invoke-ex 'global-invoke-ex) ; $r.result has global cell (always)
	   `(jmp short ,l2)		; Since ,TEMP is dead following timer interrupt
           ))))

(define-sassy-instr (ia86.t_setrtn_invoke n)
  (let ()
    (cond ((unsafe-code) ;; (see notes in unsafe version)
           (ia86.timer_check)
           (ia86.storer 0 $r.result)
           `(mov ,$r.temp (& ,$r.result ,(+ (- $tag.procedure-tag) $proc.codevector)))
           `(add ,$r.cont 4)
           `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
           (ia86.const2regf $r.result (fixnum n))
           (ia86.align-after 
            `(call ,$r.temp)))
          (else 
           (let ((l0 (fresh-label))
                 (l1 (fresh-label)))
             `(dec (dword (& ,$r.globals ,$g.timer)))
             `(jnz short ,l1)
             `(label ,l0)
             (ia86.mcall $m.invoke-ex 'invoke-ex)
             `(label ,l1)
             `(lea ,$r.temp (& ,$r.result ,(- $tag.procedure-tag)))
             `(test ,$r.temp.low ,$tag.tagmask)
             `(jnz short ,l0)
             `(mov ,$r.temp (& ,$r.temp ,$proc.codevector))
             (ia86.storer 0 $r.result)
             `(add ,$r.cont 4)
             (ia86.const2regf $r.result (fixnum n))
             `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
             (ia86.align-after
              `(call ,$r.temp))
             )))))

(define-sassy-instr (ia86.t_setrtn_branch ly)
  (let ()
    (ia86.timer_check)
    `(add ,$r.cont 4)
    (ia86.align-after
     `(call ,(t_label ly)))))

(define-sassy-instr (ia86.t_setrtn_skip ly)
  (let ()
    `(add ,$r.cont 4)
    (ia86.align-after
     `(call ,(t_label ly)))))

(define-sassy-instr (ia86.t_apply x y)
  (ia86.timer_check)
  (ia86.loadr	$r.temp y)
  `(mov	(& ,$r.globals ,$g.third) ,$r.temp)
  (ia86.loadr	$r.second x)
  (ia86.mcall	$m.apply 'apply)
  `(mov	,$r.temp (& ,$r.reg0 ,(+ (- $tag.procedure-tag) $proc.codevector)))
  `(add ,$r.temp ,(+ (- $tag.bytevector-tag) $bytevector.header-bytes))
  `(jmp	,$r.temp))

